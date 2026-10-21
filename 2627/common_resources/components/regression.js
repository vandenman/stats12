(() => {
  let nextPlotId = 0;

  // points is an array of [x, y] pairs. Axis limits stay fixed as sliders move.
  function regressionDemo(selector, options) {
    const root = document.querySelector(selector);
    if (!root) return;

    const points = options.points;
    const { xmin, xmax, ymin, ymax } = options;
    const svg = root.querySelector("svg");
    const result = root.querySelector(".regression-error-total");
    const interceptInput = root.querySelector('[data-param="intercept"]');
    const slopeInput = root.querySelector('[data-param="slope"]');

    const width = 900;
    const height = 480;
    const margin = { left: 80, right: 20, top: 20, bottom: 65 };
    const plotWidth = width - margin.left - margin.right;
    const plotHeight = height - margin.top - margin.bottom;
    const sx = x => margin.left + (x - xmin) / (xmax - xmin) * plotWidth;
    const sy = y => margin.top + (ymax - y) / (ymax - ymin) * plotHeight;
    const clipId = `regression-clip-${nextPlotId++}`;

    const xTicks = options.xTicks ?? Array.from(
      { length: 6 }, (_, i) => xmin + (xmax - xmin) * i / 5);
    const yTicks = options.yTicks ?? Array.from(
      { length: 6 }, (_, i) => ymin + (ymax - ymin) * i / 5);

    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", options.ariaLabel ??
      "Scatter plot with an adjustable regression line and vertical errors");
    svg.innerHTML = `
      <defs>
        <clipPath id="${clipId}">
          <rect x="${sx(xmin)}" y="${sy(ymax)}" width="${plotWidth}" height="${plotHeight}"></rect>
        </clipPath>
      </defs>
      <line class="regression-axis" x1="${sx(xmin)}" x2="${sx(xmax)}"
        y1="${sy(ymin)}" y2="${sy(ymin)}"></line>
      <line class="regression-axis" x1="${sx(xmin)}" x2="${sx(xmin)}"
        y1="${sy(ymin)}" y2="${sy(ymax)}"></line>
      ${xTicks.map(x => `
        <line class="regression-tick" x1="${sx(x)}" x2="${sx(x)}"
          y1="${sy(ymin)}" y2="${sy(ymin) + 7}"></line>
        <text class="regression-label" x="${sx(x)}" y="${sy(ymin) + 29}"
          text-anchor="middle">${Number(x.toFixed(2))}</text>
      `).join("")}
      ${yTicks.map(y => `
        <line class="regression-tick" x1="${sx(xmin) - 7}" x2="${sx(xmin)}"
          y1="${sy(y)}" y2="${sy(y)}"></line>
        <text class="regression-label" x="${sx(xmin) - 13}" y="${sy(y) + 6}"
          text-anchor="end">${Number(y.toFixed(2))}</text>
      `).join("")}
      <text class="regression-label" x="${sx((xmin + xmax) / 2)}"
        y="${height - 4}" text-anchor="middle">${options.xLabel ?? "x"}</text>
      <text class="regression-label" x="20" y="${sy((ymin + ymax) / 2)}"
        text-anchor="middle" transform="rotate(-90 20 ${sy((ymin + ymax) / 2)})">${options.yLabel ?? "y"}</text>
      <g class="regression-errors" clip-path="url(#${clipId})"></g>
      <g class="regression-line-group" clip-path="url(#${clipId})"></g>
      ${points.map(([x, y]) => `
        <circle class="regression-point" cx="${sx(x)}" cy="${sy(y)}" r="5"></circle>
      `).join("")}
    `;

    const errors = svg.querySelector(".regression-errors");
    const line = svg.querySelector(".regression-line-group");

    function draw() {
      const intercept = Number(interceptInput.value);
      const slope = Number(slopeInput.value);
      const predict = x => intercept + slope * x;

      root.querySelector('[data-value="intercept"]').textContent = intercept.toFixed(1);
      root.querySelector('[data-value="slope"]').textContent = slope.toFixed(2);

      errors.innerHTML = points.map(([x, y]) => `
        <line class="regression-error" x1="${sx(x)}" x2="${sx(x)}"
          y1="${sy(y)}" y2="${sy(predict(x))}"></line>
      `).join("");
      line.innerHTML = `
        <line class="regression-line" x1="${sx(xmin)}" y1="${sy(predict(xmin))}"
          x2="${sx(xmax)}" y2="${sy(predict(xmax))}"></line>
      `;

      const squaredError = points.reduce((sum, [x, y]) =>
        sum + (y - predict(x)) ** 2, 0);
      result.textContent = squaredError.toFixed(2);
    }

    interceptInput.addEventListener("input", draw);
    slopeInput.addEventListener("input", draw);
    draw();
  }

  window.regressionDemo = regressionDemo;
})();
