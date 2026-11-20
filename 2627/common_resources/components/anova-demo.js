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
  const groupNames = ["A", "B", "C"];

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
    const deviationKey = root.querySelector(".anova-deviation-key");
    const valueLabel = root.querySelector(".anova-value");
    const conclusion = root.querySelector(".anova-conclusion");
    const note = root.querySelector(".anova-note");
    const results = Object.fromEntries(Array.from(root.querySelectorAll("[data-result]"),
      element => [element.dataset.result, element]));
    let observations = initialObservations();
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
      "grand-mean": addSvg(svg, "g", { "aria-hidden": "true" }),
      "group-means": addSvg(svg, "g", { "aria-hidden": "true" }),
    };
    const grandMarker = addSvg(layers["grand-mean"], "line", {
      class: "anova-grand-mean", x1: bounds.left, x2: bounds.right,
    });
    const pointLayer = addSvg(svg, "g");
    // Spread the hit areas horizontally so tied values remain draggable.
    const pointOffsets = [-12, 60, -36, 36, -60, 12];
    const groups = observations.map((values, j) => {
      const center = bounds.left + (j + 0.5) * groupWidth;
      const color = `anova-${groupNames[j].toLowerCase()}`;
      addSvg(axes, "text", {
        class: `anova-group-label ${color}`, x: center, y: height - 9, "text-anchor": "middle",
      }, groupNames[j]);
      const marker = addSvg(layers["group-means"], "line", {
        class: `anova-group-mean ${color}`, x1: center - 78, x2: center + 78,
      });
      const between = addSvg(layers.deviations, "path", { class: "anova-between", fill: "none" });
      const points = values.map((_, i) => {
        const x = center + pointOffsets[i];
        const within = addSvg(layers.deviations, "line", { class: `anova-within ${color}`, x1: x, x2: x });
        const element = addSvg(pointLayer, "g", {
          class: `anova-point ${color}`, role: "slider", tabindex: "0",
          "aria-label": `Group ${groupNames[j]}, observation ${i + 1}`,
          "aria-orientation": "vertical", "aria-valuemin": 0, "aria-valuemax": 100,
        });
        addSvg(element, "circle", { class: "anova-hit", r: 11.5, "aria-hidden": "true" });
        addSvg(element, "circle", { class: "anova-dot", r: 7.4, "aria-hidden": "true" });
        return { x, within, element, j, i };
      });
      return { center, marker, between, points };
    });

    function setLineY(line, y1, y2 = y1) {
      line.setAttribute("y1", y1);
      line.setAttribute("y2", y2);
    }

    function render() {
      const stats = calculateAnova(observations);
      const grandY = y(stats.grandMean);
      setLineY(grandMarker, grandY);
      groups.forEach(({ center, marker, between, points }, j) => {
        const meanY = y(stats.groupMeans[j]);
        setLineY(marker, meanY);
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

      const fText = stats.f === null ? "undefined" : formatNumber.format(stats.f);
      let pText = "—";
      if (stats.p !== null) {
        pText = stats.p > 0 && stats.p < 0.001
          ? stats.p.toExponential(2)
          : formatNumber.format(stats.p);
      }
      results.fTest.textContent = `F(${stats.dfBetween}, ${stats.dfWithin}) = ${fText}, p = ${pText}`;

      let decision = "All observations are equal. The F-test is undefined.";
      if (stats.p !== null) {
        decision = stats.p < 0.05
          ? "Reject H₀: Not all population group means are equal."
          : "Do not reject H₀: Insufficient evidence that the population group means differ.";
      }
      if (conclusion.textContent !== decision) conclusion.textContent = decision;
      note.textContent = stats.f === Infinity ? "Within-group variation is zero." : "";
    }

    function showValue({ j, i }) {
      valueLabel.textContent = `Group ${groupNames[j]} · ${observations[j][i].toFixed(1)}`;
    }

    function updatePoint(point, value) {
      const clamped = Math.max(0, Math.min(100, Math.round(value * 10) / 10));
      if (observations[point.j][point.i] !== clamped) {
        observations[point.j][point.i] = clamped;
        render();
      }
      showValue(point);
    }

    function measurePlot() {
      // Reveal scales the whole slide, so client pixels and SVG units differ.
      inverseTransform = svg.getScreenCTM()?.inverse();
    }

    function pointerValue(event) {
      const local = new DOMPoint(event.clientX, event.clientY).matrixTransform(inverseTransform);
      return 100 * (bounds.bottom - local.y) / plotHeight;
    }

    function stopDrag(event) {
      if (drag?.pointerId !== event.pointerId) return;
      drag.point.element.classList.remove("is-dragging");
      drag = undefined;
    }

    const keySteps = { ArrowUp: 1, ArrowDown: -1, ArrowRight: 1, ArrowLeft: -1, PageUp: 10, PageDown: -10 };
    for (const point of groups.flatMap(group => group.points)) {
      point.element.addEventListener("focus", () => showValue(point));
      point.element.addEventListener("keydown", event => {
        if (!Object.hasOwn(keySteps, event.key) && event.key !== "Home" && event.key !== "End") return;
        event.preventDefault();
        event.stopPropagation();
        let next;
        if (event.key === "Home") next = 0;
        else if (event.key === "End") next = 100;
        else next = observations[point.j][point.i] + keySteps[event.key];
        updatePoint(point, next);
      });

      point.element.addEventListener("pointerdown", event => {
        if (drag || event.button !== 0) return;
        measurePlot();
        if (!inverseTransform) return;
        point.element.setPointerCapture(event.pointerId);
        point.element.classList.add("is-dragging");
        drag = {
          point,
          pointerId: event.pointerId,
          offset: observations[point.j][point.i] - pointerValue(event),
        };
        event.stopPropagation();
        showValue(point);
      });
    }

    svg.addEventListener("pointermove", event => {
      if (drag?.pointerId === event.pointerId) updatePoint(drag.point, pointerValue(event) + drag.offset);
    });
    svg.addEventListener("lostpointercapture", stopDrag);

    function updateLayers() {
      for (const input of layerInputs) {
        layers[input.dataset.layer].style.display = input.checked ? "" : "none";
      }
      deviationKey.hidden = !root.querySelector('[data-layer="deviations"]').checked;
    }

    for (const input of layerInputs) input.addEventListener("change", updateLayers);
    root.querySelector(".anova-reset").addEventListener("click", () => {
      if (drag) drag.point.element.releasePointerCapture(drag.pointerId);
      observations = initialObservations();
      valueLabel.textContent = "";
      render();
    });

    new ResizeObserver(measurePlot).observe(svg);
    window.addEventListener("resize", measurePlot);
    document.addEventListener("fullscreenchange", measurePlot);
    window.Reveal?.on("resize", measurePlot);
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
    module.exports = { calculateAnova, fSurvival, initialObservations };
  } else if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", mount, { once: true });
  } else {
    mount();
  }
})();
