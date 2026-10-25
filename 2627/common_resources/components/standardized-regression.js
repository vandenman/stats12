(() => {
  let nextPlotId = 0;
  const sum = values => values.reduce((total, value) => total + value, 0);
  const dot = (a, b) => sum(a.map((value, i) => value * b[i]));
  function standardize(values) {
    const mean = sum(values) / values.length;
    const centered = values.map(value => value - mean);
    const sd = Math.sqrt(dot(centered, centered) / (values.length - 1));
    return centered.map(value => value / sd);
  }

  function standardizedRegressionDemo(selector) {
    const root = document.querySelector(selector);
    if (!root) return;

    // Seeded observations, generated once. Gram–Schmidt removes the component
    // parallel to zX; both vectors use sample variance (denominator N - 1).
    let seed = 92027;
    const random = () => {
      seed = (Math.imul(1664525, seed) + 1013904223) >>> 0;
      return (seed + 0.5) / 4294967296;
    };
    const normal = () => Math.sqrt(-2 * Math.log(random())) * Math.cos(2 * Math.PI * random());
    const n = 50;
    const zX = standardize(Array.from({ length: n }, normal));
    const noise = standardize(Array.from({ length: n }, normal));
    const projection = dot(noise, zX) / dot(zX, zX);
    const u = standardize(noise.map((value, i) => value - projection * zX[i]));

    // This bound contains zY for EVERY r, including r = ±1, so axes never move.
    const bound = Math.ceil(Math.max(...zX.map((value, i) => Math.hypot(value, u[i]))) + 0.3);
    const names = ["meanX", "meanY", "sdX", "sdY", "r"];
    const inputs = Object.fromEntries(names.map(name =>
      [name, root.querySelector(`[data-param="${name}"]`)]));
    const original = root.querySelector('[data-plot="original"]');
    const standardized = root.querySelector('[data-plot="standardized"]');
    const format = (value, digits = 2) => (Math.abs(value) < 0.5 * 10 ** -digits ? 0 : value).toFixed(digits);
    // At SD x = SD y = 3 and zero means, both plots have the same geometry.
    // Moving the means can push observations outside this fixed teaching window.
    const originalRange = [-9, 9];

    function plot(svg, x, y, xRange, yRange, intercept, slope, isStandardized) {
      const width = 550, height = 365;
      const left = 72, right = 20, top = 40, bottom = 58;
      const [xmin, xmax] = xRange, [ymin, ymax] = yRange;
      const sx = value => left + (value - xmin) / (xmax - xmin) * (width - left - right);
      const sy = value => top + (ymax - value) / (ymax - ymin) * (height - top - bottom);
      const clipId = svg.dataset.clipId ??= `standardized-clip-${nextPlotId++}`;
      const ticks = range => Array.from({ length: 5 }, (_, i) => range[0] + (range[1] - range[0]) * i / 4);
      const xLabel = isStandardized ? "zₓ" : "x";
      const yLabel = isStandardized ? "zᵧ" : "y";
      svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
      svg.setAttribute("role", "img");
      svg.setAttribute("aria-label", `${isStandardized ? "Standardized" : "Original"} observations with their fitted OLS regression line`);
      svg.innerHTML = `
        <text class="regression-label" x="${width / 2}" y="24" text-anchor="middle">${isStandardized ? "Standardized" : "Original units"}</text>
        <defs><clipPath id="${clipId}"><rect x="${left}" y="${top}" width="${width - left - right}" height="${height - top - bottom}"/></clipPath></defs>
        <line class="standardized-zero" x1="${sx(0)}" x2="${sx(0)}" y1="${sy(ymin)}" y2="${sy(ymax)}"/>
        <line class="standardized-zero" x1="${sx(xmin)}" x2="${sx(xmax)}" y1="${sy(0)}" y2="${sy(0)}"/>
        <line class="regression-axis" x1="${sx(xmin)}" x2="${sx(xmax)}" y1="${sy(ymin)}" y2="${sy(ymin)}"/>
        <line class="regression-axis" x1="${sx(xmin)}" x2="${sx(xmin)}" y1="${sy(ymin)}" y2="${sy(ymax)}"/>
        ${ticks(xRange).map(value => `
          <line class="regression-tick" x1="${sx(value)}" x2="${sx(value)}" y1="${sy(ymin)}" y2="${sy(ymin) + 6}"/>
          <text class="regression-label" x="${sx(value)}" y="${sy(ymin) + 26}" text-anchor="middle">${format(value, 1)}</text>`).join("")}
        ${ticks(yRange).map(value => `
          <line class="regression-tick" x1="${left - 6}" x2="${left}" y1="${sy(value)}" y2="${sy(value)}"/>
          <text class="regression-label" x="${left - 10}" y="${sy(value) + 6}" text-anchor="end">${format(value, 1)}</text>`).join("")}
        <text class="regression-label" x="${(sx(xmin) + sx(xmax)) / 2}" y="${height - 5}" text-anchor="middle">${xLabel}</text>
        <text class="regression-label" x="18" y="${(sy(ymin) + sy(ymax)) / 2}" dominant-baseline="middle" text-anchor="middle">${yLabel}</text>
        <g clip-path="url(#${clipId})">
          <line class="regression-line" x1="${sx(xmin)}" x2="${sx(xmax)}" y1="${sy(intercept + slope * xmin)}" y2="${sy(intercept + slope * xmax)}"/>
          ${x.map((value, i) => `<circle class="regression-point" cx="${sx(value)}" cy="${sy(y[i])}" r="4"/>`).join("")}
        </g>`;
    }

    function draw() {
      const values = Object.fromEntries(names.map(name => [name, Number(inputs[name].value)]));
      const { meanX, meanY, sdX, sdY, r } = values;
      const zY = zX.map((value, i) => r * value + Math.sqrt(Math.max(0, 1 - r * r)) * u[i]);
      const x = zX.map(value => meanX + sdX * value);
      const y = zY.map(value => meanY + sdY * value);
      const b = r * sdY / sdX, a = meanY - b * meanX;
      for (const [name, value] of Object.entries(values)) {
        root.querySelector(`[data-value="${name}"]`).textContent = format(value, name === "r" ? 2 : 1);
      }
      for (const [name, value] of Object.entries({ a, b, aStd: 0, bStd: r, r2: r * r })) {
        root.querySelector(`[data-result="${name}"]`).textContent = format(value);
      }
      plot(original, x, y, originalRange, originalRange, a, b, false);
      plot(standardized, zX, zY, [-bound, bound], [-bound, bound], 0, r, true);
    }

    Object.values(inputs).forEach(input => input.addEventListener("input", draw));
    draw();
  }

  window.standardizedRegressionDemo = standardizedRegressionDemo;
})();
