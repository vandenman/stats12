const assert = require("node:assert/strict");
const { test } = require("node:test");
const { calculateAnova, fSurvival, initialObservations, shiftGroupToMean, spreadScoresToRange } = require("./anova-demo.js");

function approximately(actual, expected, tolerance = 1e-12) {
  assert.ok(Math.abs(actual - expected) <= tolerance * Math.abs(expected),
    `Expected ${actual} to equal ${expected} within relative tolerance ${tolerance}`);
}

test("F survival probabilities agree with reference F(2, 15) values", () => {
  // Reference values from R's pf(F, 2, 15, lower.tail = FALSE).
  const cases = [
    [0, 1],
    [1, 0.3911267619587493],
    [3.68232034367324, 0.05],
    [10, 0.0017384976593066122],
    [100, 2.125181337980008e-9],
    [100000, 1.155354323704441e-31],
  ];
  for (const [f, expected] of cases) approximately(fSurvival(f, 15), expected);
  assert.equal(fSurvival(Infinity, 15), 0);
});

test("the initial data give the expected ANOVA", () => {
  const stats = calculateAnova(initialObservations());
  assert.deepEqual(stats.groupMeans, [39, 50, 61]);
  assert.equal(stats.grandMean, 50);
  assert.equal(stats.ssBetween, 1452);
  assert.equal(stats.ssWithin, 292);
  assert.equal(stats.ssTotal, 1744);
  assert.equal(stats.dfBetween, 2);
  assert.equal(stats.dfWithin, 15);
  assert.equal(stats.msBetween, 726);
  approximately(stats.msWithin, 19.466666666666665);
  approximately(stats.f, 37.29452054794521);
  approximately(stats.p, 1.5092903016862328e-6);
});

test("moving a point updates all estimates and preserves the SS decomposition", () => {
  const groups = initialObservations();
  const before = calculateAnova(groups);
  groups[0][0] = 83.7;
  const after = calculateAnova(groups);
  for (const name of ["grandMean", "ssBetween", "ssWithin", "ssTotal", "msBetween", "msWithin", "f", "p"]) {
    assert.notEqual(after[name], before[name], name);
  }
  assert.notEqual(after.groupMeans[0], before.groupMeans[0]);
  assert.deepEqual(after.groupMeans.slice(1), before.groupMeans.slice(1));
  const directTotal = groups.flat().reduce((total, value) => total + (value - after.grandMean) ** 2, 0);
  approximately(after.ssBetween + after.ssWithin, directTotal);
  approximately(after.ssTotal, directTotal);
  assert.deepEqual(calculateAnova(initialObservations()), before);
});

test("F increases as means move apart and decreases as within-group spread grows", () => {
  const groups = initialObservations();
  const initial = calculateAnova(groups);
  const separated = calculateAnova(groups.map((group, j) => group.map(value => value + (j - 1) * 15)));
  assert.equal(separated.ssWithin, initial.ssWithin);
  assert.ok(separated.f > initial.f);

  const spread = calculateAnova(groups.map((group, j) => {
    const mean = initial.groupMeans[j];
    return group.map(value => mean + 2 * (value - mean));
  }));
  assert.deepEqual(spread.groupMeans, initial.groupMeans);
  assert.equal(spread.ssBetween, initial.ssBetween);
  assert.ok(spread.f < initial.f);
});

test("moving a group mean preserves within-group variation up to either plot limit", () => {
  const groups = initialObservations();
  const scores = [32.1, 36, 38, 40, 43, 45.4];
  groups[0] = scores;
  const before = calculateAnova(groups);
  const cases = [
    [before.groupMeans[0] + 5.5, [37.6, 41.5, 43.5, 45.5, 48.5, 50.9]],
    [0, [0, 3.9, 5.9, 7.9, 10.9, 13.3]],
    [100, [86.7, 90.6, 92.6, 94.6, 97.6, 100]],
  ];
  for (const [target, expected] of cases) {
    const shifted = shiftGroupToMean(scores, target);
    shifted.forEach((value, i) => approximately(value, expected[i]));
    const after = calculateAnova([shifted, ...groups.slice(1)]);
    approximately(after.ssWithin, before.ssWithin);
    assert.notEqual(after.ssBetween, before.ssBetween);
  }
  const atBothLimits = [0, 100];
  assert.equal(shiftGroupToMean(atBothLimits, 80), atBothLimits);
  assert.deepEqual(scores, [32.1, 36, 38, 40, 43, 45.4]);
});

test("changing one group's spread preserves its mean and the between-group sum of squares", () => {
  const groups = initialObservations();
  const before = calculateAnova(groups);
  const wider = spreadScoresToRange(groups[0], 26);
  assert.deepEqual(wider, [25, 33, 37, 41, 47, 51]);
  const widerStats = calculateAnova([wider, ...groups.slice(1)]);
  assert.equal(widerStats.ssBetween, before.ssBetween);
  assert.ok(widerStats.ssWithin > before.ssWithin);

  const atLimit = spreadScoresToRange(groups[0], 100);
  assert.equal(Math.min(...atLimit), 0);
  assert.ok(Math.max(...atLimit) <= 100);
  const atLimitStats = calculateAnova([atLimit, ...groups.slice(1)]);
  approximately(atLimitStats.groupMeans[0], before.groupMeans[0]);

  const shifted = shiftGroupToMean(atLimit, 45);
  approximately(calculateAnova([shifted, ...groups.slice(1)]).ssWithin, atLimitStats.ssWithin);
});

test("grand mean controls move or spread all scores together", () => {
  const groups = initialObservations();
  const before = calculateAnova(groups);
  const regroup = scores => groups.map((group, j) => scores.slice(j * group.length, (j + 1) * group.length));
  const shifted = calculateAnova(regroup(shiftGroupToMean(groups.flat(), 100)));
  assert.equal(shifted.grandMean, 84);
  assert.equal(shifted.ssBetween, before.ssBetween);
  assert.equal(shifted.ssWithin, before.ssWithin);

  const wider = calculateAnova(regroup(spreadScoresToRange(groups.flat(), 68)));
  assert.equal(wider.grandMean, before.grandMean);
  assert.equal(wider.ssBetween, 4 * before.ssBetween);
  assert.equal(wider.ssWithin, 4 * before.ssWithin);
});

test("spread can collapse to zero and expand again around a fixed mean", () => {
  const tied = spreadScoresToRange([37, 41, 51], 0);
  const center = (37 + 41 + 51) / 3;
  assert.deepEqual(tied, Array(3).fill(center));
  const wider = spreadScoresToRange(tied, 20);
  assert.deepEqual(wider, [center - 10, center, center + 10]);
  const atEdge = [100, 100, 100];
  assert.equal(spreadScoresToRange(atEdge, 20), atEdge);
});

test("zero within-group variation gives either infinite or undefined F", () => {
  const constantGroups = means => means.map(mean => Array(6).fill(mean));
  const different = calculateAnova(constantGroups([39.1, 50.1, 61.1]));
  assert.equal(different.ssWithin, 0);
  assert.equal(different.f, Infinity);
  assert.equal(different.p, 0);

  const identical = calculateAnova(constantGroups([39.1, 39.1, 39.1]));
  assert.equal(identical.ssTotal, 0);
  assert.equal(identical.f, null);
  assert.equal(identical.p, null);

  const sameMeans = calculateAnova([initialObservations()[0], [39, 39, 39], [38, 40]]);
  assert.equal(sameMeans.f, 0);
  assert.equal(sameMeans.p, 1);
});
