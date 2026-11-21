(() => {
  "use strict";

  function initialObservations() {
    return [
      [32, 36, 38, 40, 43, 45],
      [44, 47, 49, 51, 53, 56],
      [55, 58, 60, 62, 65, 66],
    ];
  }

  const sum = values => values.reduce((total, value) => total + value, 0);
  const square = value => value * value;

  function mean(values) {
    // Center the sum so equal observations have an exactly equal mean.
    const first = values[0];
    return first + sum(values.map(value => value - first)) / values.length;
  }

  function shiftLimits(values) {
    return { min: -Math.min(...values), max: 100 - Math.max(...values) };
  }

  function shiftGroupToMean(values, targetMean) {
    const { min, max } = shiftLimits(values);
    const delta = targetMean - mean(values);
    const requestedShift = delta > min && delta < max ? Math.round(delta * 10) / 10 : delta;
    // Cap the shared shift before applying it so every pair of scores stays equally far apart.
    const shift = Math.max(min, Math.min(max, requestedShift));
    return shift === 0 ? values : values.map(value => value + shift);
  }

  const scoreRange = values => Math.max(...values) - Math.min(...values);

  function spreadAroundMean(values) {
    const center = mean(values);
    const range = scoreRange(values);
    // Give fully tied scores evenly spaced offsets so they can spread apart again.
    const offsets = range === 0
      ? values.map((_, i) => (i - (values.length - 1) / 2) / (values.length - 1))
      : values.map(value => (value - center) / range);
    const lower = -Math.min(...offsets);
    const upper = Math.max(...offsets);
    const maximumRange = Math.min(lower > 0 ? center / lower : Infinity,
      upper > 0 ? (100 - center) / upper : Infinity);
    return { center, range, offsets, maximumRange };
  }

  function spreadScoresToRange(values, targetRange) {
    const { center, range, offsets, maximumRange } = spreadAroundMean(values);
    const nextRange = Math.max(0, Math.min(maximumRange, targetRange));
    if (nextRange === range) return values;
    return offsets.map(offset => Math.max(0, Math.min(100, center + offset * nextRange)));
  }

  function fSurvival(f, dfWithin) {
    // For two numerator degrees of freedom, the upper-tail beta integral
    // simplifies to (1 + 2F / dfWithin)^(-dfWithin / 2).
    return Math.exp(-dfWithin / 2 * Math.log1p(2 * f / dfWithin));
  }

  function calculateAnova(groups) {
    const observations = groups.flat();
    const groupMeans = groups.map(mean);
    const grandMean = mean(observations);
    const ssBetween = sum(groups.map((group, j) => group.length * square(groupMeans[j] - grandMean)));
    const ssWithin = sum(groups.map((group, j) => sum(group.map(value => square(value - groupMeans[j])))));
    const ssTotal = ssBetween + ssWithin;
    const dfBetween = groups.length - 1;
    const dfWithin = observations.length - groups.length;
    const msBetween = ssBetween / dfBetween;
    const msWithin = ssWithin / dfWithin;

    // Different constant group means give an infinite F. Equal constants
    // in every group give 0 / 0, so neither F nor p is defined.
    const f = ssTotal === 0 ? null : msBetween / msWithin;
    const p = f === null ? null : fSurvival(f, dfWithin);
    return { groupMeans, grandMean, ssBetween, ssWithin, ssTotal, dfBetween, dfWithin, msBetween, msWithin, f, p };
  }

  const formatNumber = new Intl.NumberFormat("en-US", { maximumSignificantDigits: 3 });
  const formatF = new Intl.NumberFormat("en-US", { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  const groupNames = ["Gryffindor", "Ravenclaw", "Slytherin"];

  function addSvg(parent, tag, attrs = {}, label) {
    const element = document.createElementNS("http://www.w3.org/2000/svg", tag);
    for (const [name, value] of Object.entries(attrs)) element.setAttribute(name, value);
    if (label !== undefined) element.textContent = label;
    parent.append(element);
    return element;
  }

  function initialize(root) {
    const svg = root.querySelector("svg");
    const layerInputs = root.querySelectorAll("[data-layer]");
    const modeButton = root.querySelector(".anova-drag-mode button");
    const deviationKey = root.querySelector(".anova-deviation-key");
    const conclusion = root.querySelector(".anova-conclusion");
    const note = root.querySelector(".anova-note");
    const results = Object.fromEntries(Array.from(root.querySelectorAll("[data-result]"),
      element => [element.dataset.result, element]));
    let observations = initialObservations();
    let adjustSpread = false;
    let drag;
    let inverseTransform;

    const { width, height } = svg.viewBox.baseVal;
    const bounds = { left: 46, right: width - 12, top: 15, bottom: height - 33 };
    const plotHeight = bounds.bottom - bounds.top;
    const groupWidth = (bounds.right - bounds.left) / groupNames.length;
    const y = value => bounds.bottom - value / 100 * plotHeight;

    const axes = addSvg(svg, "g", { "aria-hidden": "true" });
    for (const tick of [0, 20, 40, 60, 80, 100]) {
      const position = y(tick);
      addSvg(axes, "line", { class: "anova-grid", x1: bounds.left, x2: bounds.right, y1: position, y2: position });
      addSvg(axes, "text", { x: bounds.left - 10, y: position, dy: "0.33em", "text-anchor": "end" }, tick);
    }
    addSvg(axes, "path", {
      class: "anova-axis",
      d: `M ${bounds.left} ${bounds.top} V ${bounds.bottom} H ${bounds.right}`,
      fill: "none",
    });

    const layers = {
      deviations: addSvg(svg, "g", { "aria-hidden": "true" }),
      "grand-mean": addSvg(svg, "g"),
      "group-means": addSvg(svg, "g"),
    };

    function addMeanSlider(layer, classes, lineClass, x1, x2) {
      const element = addSvg(layer, "g", {
        class: `anova-mean-slider ${classes}`, role: "slider", tabindex: "0", "aria-orientation": "vertical",
      });
      const ends = { x1, x2, "aria-hidden": "true" };
      const hit = addSvg(element, "line", { ...ends, class: "anova-mean-hit" });
      const marker = addSvg(element, "line", { ...ends, class: lineClass });
      return { element, hit, marker };
    }

    const grandSlider = addMeanSlider(layers["grand-mean"], "anova-grand-slider", "anova-grand-mean",
      bounds.left, bounds.right);
    const pointLayer = addSvg(svg, "g");
    // Spread the hit areas horizontally so tied values remain draggable.
    const pointOffsets = [-12, 60, -36, 36, -60, 12];
    const groups = observations.map((values, j) => {
      const center = bounds.left + (j + 0.5) * groupWidth;
      const name = groupNames[j];
      const groupClass = `anova-${name.toLowerCase()}`;
      addSvg(axes, "text", {
        class: `anova-group-label ${groupClass}`, x: center, y: height - 9, "text-anchor": "middle",
      }, name);
      const meanSlider = addMeanSlider(layers["group-means"], groupClass, "anova-group-mean", center - 78, center + 78);
      const between = addSvg(layers.deviations, "path", { class: "anova-between", fill: "none" });
      const points = values.map((_, i) => {
        const x = center + pointOffsets[i];
        const within = addSvg(layers.deviations, "line", { class: `anova-within ${groupClass}`, x1: x, x2: x });
        const element = addSvg(pointLayer, "g", {
          class: `anova-point ${groupClass}`, role: "slider", tabindex: "0",
          "aria-label": `${name}, observation ${i + 1}`,
          "aria-orientation": "vertical", "aria-valuemin": 0, "aria-valuemax": 100,
        });
        addSvg(element, "circle", { class: "anova-hit", r: 11.5, "aria-hidden": "true" });
        addSvg(element, "circle", { class: "anova-dot", r: 7.4, "aria-hidden": "true" });
        return { x, within, element, i };
      });
      return { center, meanSlider, between, points };
    });

    function setLineY(line, y1, y2 = y1) {
      line.setAttribute("y1", y1);
      line.setAttribute("y2", y2);
    }

    function renderMeanSlider({ element, hit, marker }, meanLabel, spreadLabel, values, center) {
      if (adjustSpread) {
        const { range, maximumRange } = spreadAroundMean(values);
        element.setAttribute("aria-label", `${spreadLabel} (score range)`);
        element.setAttribute("aria-valuemin", "0");
        element.setAttribute("aria-valuemax", maximumRange.toFixed(1));
        element.setAttribute("aria-valuenow", range.toFixed(1));
      } else {
        const { min, max } = shiftLimits(values);
        element.setAttribute("aria-label", meanLabel);
        element.setAttribute("aria-valuemin", (center + min).toFixed(1));
        element.setAttribute("aria-valuemax", (center + max).toFixed(1));
        element.setAttribute("aria-valuenow", center.toFixed(1));
      }
      setLineY(hit, y(center));
      setLineY(marker, y(center));
    }

    function render() {
      const stats = calculateAnova(observations);
      const grandY = y(stats.grandMean);
      renderMeanSlider(grandSlider, "Grand mean", "Overall spread", observations.flat(), stats.grandMean);
      groups.forEach(({ center, meanSlider, between, points }, j) => {
        const groupMean = stats.groupMeans[j];
        const meanY = y(groupMean);
        const name = groupNames[j];
        renderMeanSlider(meanSlider, `${name} mean`, `${name} spread`, observations[j], groupMean);
        const x = center + 76;
        between.setAttribute("d", `M ${x - 4} ${meanY} h 8 m -4 0 V ${grandY} m -4 0 h 8`);
        points.forEach(({ x, within, element, i }) => {
          const value = observations[j][i];
          const pointY = y(value);
          element.setAttribute("transform", `translate(${x} ${pointY})`);
          element.setAttribute("aria-valuenow", value.toFixed(1));
          setLineY(within, pointY, meanY);
        });
      });

      for (const name of ["ssBetween", "ssWithin", "ssTotal"]) {
        results[name].textContent = formatNumber.format(stats[name]);
      }

      const fText = stats.f === null ? "undefined" : formatF.format(stats.f);
      results.fTest.textContent = `F(${stats.dfBetween}, ${stats.dfWithin}) = ${fText}`;
      let pText = "p = —";
      if (stats.p !== null) {
        pText = stats.p < 0.001 ? "p <.001" : `p = ${formatNumber.format(stats.p)}`;
      }
      results.pValue.textContent = pText;

      let decision = "All observations are equal. The F-test is undefined.";
      if (stats.p !== null) {
        decision = stats.p < 0.05
          ? "Reject H₀: Not all population group means are equal."
          : "Do not reject H₀: Insufficient evidence that the population group means differ.";
      }
      if (conclusion.textContent !== decision) conclusion.textContent = decision;
      note.textContent = stats.f === Infinity ? "Within-group variation is zero." : "";
    }

    function updatePoint(j, i, value) {
      const clamped = Math.max(0, Math.min(100, Math.round(value * 10) / 10));
      if (observations[j][i] !== clamped) {
        const scores = observations[j].slice();
        scores[i] = clamped;
        observations[j] = scores;
        render();
      }
    }

    function updateScores(scores, groupIndex) {
      const current = groupIndex === undefined ? observations.flat() : observations[groupIndex];
      if (scores.every((score, i) => score === current[i])) return;
      if (groupIndex === undefined) {
        let start = 0;
        observations = observations.map(group => {
          const end = start + group.length;
          const updated = scores.slice(start, end);
          start = end;
          return updated;
        });
      } else {
        observations[groupIndex] = scores;
      }
      render();
    }

    function measurePlot() {
      // Reveal scales the whole slide, so client pixels and SVG units differ.
      inverseTransform = svg.getScreenCTM()?.inverse();
    }

    function pointerValue(event) {
      const local = new DOMPoint(event.clientX, event.clientY).matrixTransform(inverseTransform);
      return 100 * (bounds.bottom - local.y) / plotHeight;
    }

    function stopDrag({ pointerId }) {
      if (drag?.pointerId !== pointerId) return;
      const { element } = drag;
      drag = undefined;
      element.classList.remove("is-dragging");
      if (element.hasPointerCapture(pointerId)) element.releasePointerCapture(pointerId);
    }

    const keySteps = { ArrowUp: 1, ArrowDown: -1, ArrowRight: 1, ArrowLeft: -1, PageUp: 10, PageDown: -10 };
    function bindSlider(element, getAdjustment) {
      element.addEventListener("keydown", event => {
        if (!Object.hasOwn(keySteps, event.key) && event.key !== "Home" && event.key !== "End") return;
        event.preventDefault();
        event.stopPropagation();
        if (drag) stopDrag(drag);
        const { value, updateValue } = getAdjustment();
        let next;
        if (event.key === "Home") next = 0;
        else if (event.key === "End") next = 100;
        else next = value + keySteps[event.key];
        updateValue(next);
      });

      element.addEventListener("pointerdown", event => {
        if (drag || event.button !== 0) return;
        measurePlot();
        if (!inverseTransform) return;
        const { value, updateValue } = getAdjustment();
        element.setPointerCapture(event.pointerId);
        element.classList.add("is-dragging");
        drag = {
          element,
          updateValue,
          pointerId: event.pointerId,
          offset: value - pointerValue(event),
        };
        event.stopPropagation();
      });
    }

    function bindMeanSlider(element, groupIndex) {
      bindSlider(element, () => {
        // Keep the starting shape for the whole drag, even when its range temporarily reaches zero.
        const scores = groupIndex === undefined ? observations.flat() : observations[groupIndex];
        const adjust = adjustSpread ? spreadScoresToRange : shiftGroupToMean;
        return {
          value: adjustSpread ? scoreRange(scores) : mean(scores),
          updateValue: target => updateScores(adjust(scores, target), groupIndex),
        };
      });
    }

    bindMeanSlider(grandSlider.element);
    groups.forEach(({ meanSlider, points }, j) => {
      bindMeanSlider(meanSlider.element, j);
      for (const { element, i } of points) {
        bindSlider(element, () => ({ value: observations[j][i], updateValue: value => updatePoint(j, i, value) }));
      }
    });

    svg.addEventListener("pointermove", event => {
      if (drag?.pointerId === event.pointerId) drag.updateValue(pointerValue(event) + drag.offset);
    });
    for (const type of ["pointerup", "pointercancel", "lostpointercapture"]) {
      svg.addEventListener(type, stopDrag);
    }

    function updateLayers() {
      for (const input of layerInputs) {
        const layer = layers[input.dataset.layer];
        if (!input.checked && drag && layer.contains(drag.element)) stopDrag(drag);
        layer.style.display = input.checked ? "" : "none";
      }
      deviationKey.hidden = !root.querySelector('[data-layer="deviations"]').checked;
    }

    for (const input of layerInputs) input.addEventListener("change", updateLayers);
    function updateModeLabel() {
      const active = adjustSpread ? "Adjust spread" : "Adjust means";
      const inactive = adjustSpread ? "Adjust means" : "Adjust spread";
      modeButton.textContent = active;
      modeButton.setAttribute("aria-label", `Drag mode. ${active}. Switch to ${inactive}`);
    }
    modeButton.addEventListener("click", () => {
      if (drag) stopDrag(drag);
      adjustSpread = !adjustSpread;
      updateModeLabel();
      render();
    });
    root.querySelector(".anova-controls").addEventListener("keydown", event => {
      // Let Space activate the focused control without advancing the slide.
      if (event.key === " ") event.stopPropagation();
    });
    root.querySelector(".anova-reset").addEventListener("click", () => {
      if (drag) stopDrag(drag);
      observations = initialObservations();
      adjustSpread = false;
      updateModeLabel();
      for (const input of layerInputs) input.checked = input.defaultChecked;
      updateLayers();
      render();
    });

    new ResizeObserver(measurePlot).observe(svg);
    window.addEventListener("resize", measurePlot);
    document.addEventListener("fullscreenchange", measurePlot);
    window.Reveal?.on("resize", measurePlot);
    updateModeLabel();
    updateLayers();
    render();
  }

  function mount() {
    const reveal = window.Reveal;
    const initialized = new WeakSet();
    function mountWidgets(container) {
      for (const root of container.querySelectorAll(".anova-demo")) {
        if (initialized.has(root)) continue;
        initialize(root);
        initialized.add(root);
      }
    }

    if (reveal) {
      const mountSlide = ({ currentSlide }) => mountWidgets(currentSlide);
      reveal.on("ready", mountSlide);
      reveal.on("slidechanged", mountSlide);
      if (reveal.isReady()) mountWidgets(reveal.getCurrentSlide());
    } else {
      mountWidgets(document);
    }
  }

  if (typeof module === "object" && module.exports) {
    module.exports = { calculateAnova, fSurvival, initialObservations, shiftGroupToMean, spreadScoresToRange };
  } else if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", mount, { once: true });
  } else {
    mount();
  }
})();
