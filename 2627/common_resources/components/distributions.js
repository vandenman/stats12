(() => {

  // Actual distribution functions from jStat.
  const dnorm = (x, mean = 0, sd = 1) =>
    jStat.normal.pdf(x, mean, sd);

  const pnorm = (x, mean = 0, sd = 1) =>
    jStat.normal.cdf(x, mean, sd);

  const qnorm = (p, mean = 0, sd = 1) =>
    jStat.normal.inv(p, mean, sd);

  const dshash = (x,location = 0, scale = 1, skew = 0, tail = 1) => {

    const y = (x - location) / scale;
    const S = Math.sinh(tail * Math.asinh(y) - skew);

    return (
      tail /
      (scale * Math.sqrt(2 * Math.PI)) *
      Math.sqrt((1 + S * S) / (1 + y * y)) *
      Math.exp(-0.5 * S * S)
    );
  };

  function linspace(a, b, n) {
    return Array.from(
      { length: n },
      (_, i) => a + i * (b - a) / (n - 1)
    );
  }

  function pathFromPoints(points) {
    return points
      .map((p, i) =>
        `${i === 0 ? "M" : "L"}${p[0].toFixed(2)},${p[1].toFixed(2)}`
      )
      .join(" ");
  }

  function normalDemo(selector, options = {}) {
    const root = document.querySelector(selector);
    if (!root) return;

    const svg = root.querySelector("svg");
    const result = root.querySelector(".normal-result");

    const width = 900;
    const height = options.height ?? 300;

    const margin = {
      left: 55,
      right: 15,
      top: 8,
      bottom: 42
    };

    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);

    const controls = {};

    root.querySelectorAll("[data-param]").forEach(input => {
      controls[input.dataset.param] = input;

      const output = root.querySelector(
        `[data-value="${input.dataset.param}"]`
      );

      const updateLabel = () => {
        if (output) {
          output.textContent = Number(input.value).toFixed(1);
        }
      };

      input.addEventListener("input", () => {
        updateLabel();
        draw();
      });

      updateLabel();
    });

    function value(name, fallback) {
      return controls[name]
        ? Number(controls[name].value)
        : fallback;
    }

    function draw() {
      const mean = value("mean", options.mean ?? 0);
      const sd = value("sd", options.sd ?? 1);

      const lower = value(
        "lower",
        options.lower ?? -Infinity
      );

      const upper = value(
        "upper",
        options.upper ?? Infinity
      );

      const xmin = options.xmin ?? -5;
      const xmax = options.xmax ?? 5;
      const ymax = options.ymax ?? 0.9;

      const plotWidth =
        width - margin.left - margin.right;

      const plotHeight =
        height - margin.top - margin.bottom;

      const sx = x =>
        margin.left +
        (x - xmin) / (xmax - xmin) * plotWidth;

      const sy = y =>
        margin.top +
        plotHeight -
        y / ymax * plotHeight;

      const xs = linspace(xmin, xmax, 400);

      const points = xs.map(x => [
        sx(x),
        sy(dnorm(x, mean, sd))
      ]);

      const shadedXs = xs.filter(
        x => x >= lower && x <= upper
      );

      const shaded = shadedXs.map(x => [
        sx(x),
        sy(dnorm(x, mean, sd))
      ]);

      let areaPath = "";

      if (shaded.length > 0) {
        areaPath =
          `M${sx(shadedXs[0])},${sy(0)} ` +
          pathFromPoints(shaded).replace(/^M/, "L") +
          ` L${sx(shadedXs.at(-1))},${sy(0)} Z`;
      }

      const ticks =
        options.ticks ?? [-4, -2, 0, 2, 4];

      const tickSVG = ticks.map(x => `
        <line
          class="normal-tick"
          x1="${sx(x)}"
          x2="${sx(x)}"
          y1="${sy(0)}"
          y2="${sy(0) + 8}">
        </line>

        <text
          class="normal-label"
          x="${sx(x)}"
          y="${sy(0) + 30}"
          text-anchor="middle">
          ${x}
        </text>
      `).join("");

      const bounds = [];

      if (Number.isFinite(lower)) {
        bounds.push(`
          <line
            class="normal-bound"
            x1="${sx(lower)}"
            x2="${sx(lower)}"
            y1="${sy(0)}"
            y2="${sy(dnorm(lower, mean, sd))}">
          </line>
        `);
      }

      if (Number.isFinite(upper)) {
        bounds.push(`
          <line
            class="normal-bound"
            x1="${sx(upper)}"
            x2="${sx(upper)}"
            y1="${sy(0)}"
            y2="${sy(dnorm(upper, mean, sd))}">
          </line>
        `);
      }

      svg.innerHTML = `
        <line
          class="normal-axis"
          x1="${sx(xmin)}"
          x2="${sx(xmax)}"
          y1="${sy(0)}"
          y2="${sy(0)}">
        </line>

        ${tickSVG}

        ${
          areaPath
            ? `<path class="normal-area" d="${areaPath}"></path>`
            : ""
        }

        <path
          class="normal-curve"
          d="${pathFromPoints(points)}">
        </path>

        ${bounds.join("")}
      `;

      if (result) {
        const lo =
          Number.isFinite(lower)
            ? pnorm(lower, mean, sd)
            : 0;

        const hi =
          Number.isFinite(upper)
            ? pnorm(upper, mean, sd)
            : 1;

        const probability = hi - lo;

        if (
          Number.isFinite(lower) &&
          Number.isFinite(upper)
        ) {
          result.innerHTML =
            `P(${lower.toFixed(1)} ≤ X ≤ ${upper.toFixed(1)}) = ` +
            `<strong>${probability.toFixed(3)}</strong>`;
        } else if (Number.isFinite(upper)) {
          result.innerHTML =
            `P(X ≤ ${upper.toFixed(1)}) = ` +
            `<strong>${probability.toFixed(3)}</strong>`;
        } else {
          result.innerHTML = "";
        }
      }
    }

    draw();
  }

  function shashDemo(selector, options = {}) {
    const root = document.querySelector(selector);
    if (!root) return;

    const svg = root.querySelector("svg");

    const width = 900;
    const height = options.height ?? 300;

    const margin = {
      left: 55,
      right: 15,
      top: 8,
      bottom: 42
    };

    svg.setAttribute("viewBox", `0 0 ${width} ${height}`);

    const controls = {};

    root.querySelectorAll("[data-param]").forEach(input => {
      controls[input.dataset.param] = input;

      const output = root.querySelector(
        `[data-value="${input.dataset.param}"]`
      );

      const updateLabel = () => {
        if (output) {
          output.textContent = Number(input.value).toFixed(1);
        }
      };

      input.addEventListener("input", () => {
        updateLabel();
        draw();
      });

      updateLabel();
    });

    function value(name, fallback) {
      return controls[name]
        ? Number(controls[name].value)
        : fallback;
    }

    function draw() {
      const location = value("location", 0);
      const scale = value("scale", 1);
      const skew = value("skew", 0);
      const tail = value("tail", 1);

      const xmin = options.xmin ?? -6;
      const xmax = options.xmax ?? 6;
      const ymax = options.ymax ?? 0.9;

      const plotWidth =
        width - margin.left - margin.right;

      const plotHeight =
        height - margin.top - margin.bottom;

      const sx = x =>
        margin.left +
        (x - xmin) / (xmax - xmin) * plotWidth;

      const sy = y =>
        margin.top +
        plotHeight -
        y / ymax * plotHeight;

      const xs = linspace(xmin, xmax, 400);

      const shashPoints = xs.map(x => [
        sx(x),
        sy(dshash(x, location, scale, skew, tail))
      ]);

      const normalPoints = xs.map(x => [
        sx(x),
        sy(dnorm(x, location, scale))
      ]);

      const ticks =
        options.ticks ?? [-4, -2, 0, 2, 4];

      const tickSVG = ticks.map(x => `
        <line
          class="normal-tick"
          x1="${sx(x)}"
          x2="${sx(x)}"
          y1="${sy(0)}"
          y2="${sy(0) + 8}">
        </line>

        <text
          class="normal-label"
          x="${sx(x)}"
          y="${sy(0) + 30}"
          text-anchor="middle">
          ${x}
        </text>
      `).join("");

      svg.innerHTML = `
        <line
          class="normal-axis"
          x1="${sx(xmin)}"
          x2="${sx(xmax)}"
          y1="${sy(0)}"
          y2="${sy(0)}">
        </line>

        ${tickSVG}

        <path
          d="${pathFromPoints(normalPoints)}"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-dasharray="7 6"
          opacity="0.35">
        </path>

        <path
          class="normal-curve"
          d="${pathFromPoints(shashPoints)}">
        </path>
      `;
    }

    draw();
  }

  window.shashDemo = shashDemo;
  window.dshash = dshash;

  window.normalDemo = normalDemo;
  window.dnorm = dnorm;
  window.pnorm = pnorm;
  window.qnorm = qnorm;

})();