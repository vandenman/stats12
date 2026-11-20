const assert = require("node:assert/strict");
const { test } = require("node:test");
const { calculateAnova, fSurvival, initialObservations } = require("./anova-demo.js");

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
