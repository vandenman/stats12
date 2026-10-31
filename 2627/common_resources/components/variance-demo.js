(() => {
  // The same irregular x positions and residual pattern are used at every setting.
  function randomGenerator(seed) {
    return () => {
      seed = (seed + 0x6D2B79F5) | 0;
      let value = Math.imul(seed ^ seed >>> 15, 1 | seed);
      value ^= value + Math.imul(value ^ value >>> 7, 61 | value);
      return ((value ^ value >>> 14) >>> 0) / 4294967296;
    };
  }

  function varianceDemo(selector) {
    // Accept an element so repeated includes each initialize their own demo.
    const root = typeof selector === "string" ? document.querySelector(selector) : selector;
    if (!root) return;

    const random = randomGenerator(92027);
    const n = 50;
    const x = Array.from({ length: n }, () => 0.3 + 9.4 * random());
    const xMean = x.reduce((sum, value) => sum + value, 0) / n;
    const xc = x.map(value => value - xMean);
    const sxx = xc.reduce((sum, value) => sum + value * value, 0);

    // Remove the components parallel to the intercept and x. The resulting
    // residuals make the displayed line the exact least-squares fit.
    const noise = Array.from({ length: n }, () => random() - 0.5);
    const noiseMean = noise.reduce((sum, value) => sum + value, 0) / n;
    const projection = noise.reduce((sum, value, i) =>
      sum + (value - noiseMean) * xc[i], 0) / sxx;
    const residual = noise.map((value, i) => value - noiseMean - projection * xc[i]);
    const residualLength = Math.sqrt(residual.reduce((sum, value) => sum + value * value, 0));
    const unitResidual = residual.map(value => value / residualLength);

    const svg = root.querySelector("svg");
    const inputs = Object.fromEntries(["mean", "variance", "r2"].map(name =>
      [name, root.querySelector(`[data-param="${name}"]`)]));
    const width = 900, height = 480;
    const margin = { left: 80, right: 20, top: 20, bottom: 65 };
    const plotWidth = width - margin.left - margin.right;
    const plotHeight = height - margin.top - margin.bottom;
    const sx = value => margin.left + value / 10 * plotWidth;

    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", "Scatter plot of 50 observations and their fitted regression line");

    function draw() {
      const mean = Number(inputs.mean.value);
      const variance = Number(inputs.variance.value);
      const r2 = Number(inputs.r2.value);
      const tss = (n - 1) * variance;
      const rss = r2 * tss;
      const sse = tss - rss;
      const slope = Math.sqrt(rss / sxx);
      const fitted = x.map(value => mean + slope * (value - xMean));
      const y = fitted.map((value, i) => value + Math.sqrt(sse) * unitResidual[i]);
      const lineAt = value => mean + slope * (value - xMean);

      // The default scale is 0–10; expand only when a chosen setting needs it.
      const lower = Math.min(0, ...y, lineAt(0));
      const upper = Math.max(10, ...y, lineAt(10));
      const ymin = Math.floor(lower);
      const ymax = Math.ceil(upper);
      const sy = value => margin.top + (ymax - value) / (ymax - ymin) * plotHeight;
      const yTicks = Array.from({ length: ymax - ymin + 1 }, (_, i) => ymin + i)
        .filter(value => (value - ymin) % 2 === 0 || value === ymax);

      for (const [name, value] of Object.entries({ mean, variance, r2 })) {
        root.querySelector(`[data-value="${name}"]`).textContent = value.toFixed(name === "r2" ? 2 : 1);
      }
      for (const [name, value] of Object.entries({ tss, sse, rss })) {
        root.querySelector(`[data-result="${name}"]`).textContent = value.toFixed(2);
      }

      svg.innerHTML = `
        <line class="regression-axis" x1="${sx(0)}" x2="${sx(10)}"
          y1="${sy(ymin)}" y2="${sy(ymin)}"></line>
        <line class="regression-axis" x1="${sx(0)}" x2="${sx(0)}"
          y1="${sy(ymin)}" y2="${sy(ymax)}"></line>
        ${[0, 2, 4, 6, 8, 10].map(value => `
          <line class="regression-tick" x1="${sx(value)}" x2="${sx(value)}"
            y1="${sy(ymin)}" y2="${sy(ymin) + 7}"></line>
          <text class="regression-label" x="${sx(value)}" y="${sy(ymin) + 29}"
            text-anchor="middle">${value}</text>
        `).join("")}
        ${yTicks.map(value => `
          <line class="regression-tick" x1="${sx(0) - 7}" x2="${sx(0)}"
            y1="${sy(value)}" y2="${sy(value)}"></line>
          <text class="regression-label" x="${sx(0) - 13}" y="${sy(value) + 6}"
            text-anchor="end">${value}</text>
        `).join("")}
        <text class="regression-label" x="${sx(5)}" y="${height - 4}"
          text-anchor="middle">x</text>
        <text class="regression-label" x="20" y="${sy((ymin + ymax) / 2)}"
          text-anchor="middle" transform="rotate(-90 20 ${sy((ymin + ymax) / 2)})">y</text>
        <line class="regression-line" x1="${sx(0)}" y1="${sy(lineAt(0))}"
          x2="${sx(10)}" y2="${sy(lineAt(10))}"></line>
        ${x.map((value, i) => `
          <circle class="regression-point" cx="${sx(value)}" cy="${sy(y[i])}" r="5"></circle>
        `).join("")}
      `;
    }

    Object.values(inputs).forEach(input => input.addEventListener("input", draw));
    draw();
  }

  window.varianceDemo = varianceDemo;
})();
