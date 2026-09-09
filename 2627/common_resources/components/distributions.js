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

  const rshash = (
    n,
    location = 0,
    scale = 1,
    skew = 0,
    tail = 1
  ) => {
    return Array.from({ length: n }, () => {
      const u = jStat.normal.sample(0, 1);

      const z =
        Math.sinh(
          (Math.asinh(u) - skew) / tail
        );

      return location + scale * z;
    });
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

  function bivariateNormalDemo(selector, options = {}) {
    const root = document.querySelector(selector);
    if (!root) return;

    const svg = root.querySelector("svg");

    // const width = 900;
    // const height = options.height ?? 330;
    // const margin = {
    //   left: 55,
    //   right: 20,
    //   top: 10,
    //   bottom: 45
    // };

    const width = 600;
    const height = 600;

    const margin = {
      left: 55,
      right: 20,
      top: 20,
      bottom: 55
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

    // Fixed standard-normal cloud.
    // Keeping the same underlying points makes slider changes visually smooth.
    function randomNormal() {
      let u = 0;
      let v = 0;

      while (u === 0) u = Math.random();
      while (v === 0) v = Math.random();

      return Math.sqrt(-2 * Math.log(u)) *
        Math.cos(2 * Math.PI * v);
    }

    const nPoints = options.nPoints ?? 250;

    const basePoints = Array.from(
      { length: nPoints },
      () => [randomNormal(), randomNormal()]
    );

    function draw() {
      const mu1 = value("mu1", 0);
      const mu2 = value("mu2", 0);

      const sigma1 = value("sigma1", 1);
      const sigma2 = value("sigma2", 1);

      const rho = value("rho", 0);

      const xmin = options.xmin ?? -5;
      const xmax = options.xmax ?? 5;
      const ymin = options.ymin ?? -5;
      const ymax = options.ymax ?? 5;

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
        (y - ymin) / (ymax - ymin) * plotHeight;

      /*
        Cholesky factor for

        Sigma =
        [ sigma1^2               rho sigma1 sigma2 ]
        [ rho sigma1 sigma2      sigma2^2          ]

        A convenient factor is

        L =
        [ sigma1                         0 ]
        [ rho sigma2    sigma2 sqrt(1-rho^2) ]
      */

      const transformedPoints = basePoints.map(([z1, z2]) => {
        const x =
          mu1 +
          sigma1 * z1;

        const y =
          mu2 +
          sigma2 *
            (
              rho * z1 +
              Math.sqrt(1 - rho * rho) * z2
            );

        return [x, y];
      });

      /*
        Eigenvalues/eigenvectors of the 2x2 covariance matrix.
        These determine the orientation and radii of the contours.
      */

      const a = sigma1 * sigma1;
      const b = rho * sigma1 * sigma2;
      const d = sigma2 * sigma2;

      const trace = a + d;

      const rootTerm =
        Math.sqrt(
          (a - d) * (a - d) +
          4 * b * b
        );

      const lambda1 =
        (trace + rootTerm) / 2;

      const lambda2 =
        (trace - rootTerm) / 2;

      const angle =
        0.5 * Math.atan2(
          2 * b,
          a - d
        );

      /*
        Mahalanobis radii.
        1, 2, 3 SD-like contour levels.
      */
      const contourLevels =
        options.contours ?? [1, 2, 3];

      function ellipsePoints(level) {
        const r1 =
          level * Math.sqrt(lambda1);

        const r2 =
          level * Math.sqrt(lambda2);

        const cosA = Math.cos(angle);
        const sinA = Math.sin(angle);

        const n = 160;

        return Array.from(
          { length: n + 1 },
          (_, i) => {
            const t =
              2 * Math.PI * i / n;

            const u =
              r1 * Math.cos(t);

            const v =
              r2 * Math.sin(t);

            const x =
              mu1 +
              u * cosA -
              v * sinA;

            const y =
              mu2 +
              u * sinA +
              v * cosA;

            return [sx(x), sy(y)];
          }
        );
      }

      const contourSVG =
        contourLevels.map(level => {
          const points =
            ellipsePoints(level);

          return `
            <path
              class="bvn-contour"
              d="${pathFromPoints(points)}"
              opacity="${1 - 0.18 * (level - 1)}">
            </path>
          `;
        }).join("");

      const pointSVG =
        transformedPoints.map(([x, y]) => `
          <circle
            class="bvn-point"
            cx="${sx(x)}"
            cy="${sy(y)}"
            r="3">
          </circle>
        `).join("");

      const ticks =
        options.ticks ?? [-4, -2, 0, 2, 4];

      const xTicks =
        ticks.map(x => `
          <line
            class="bvn-tick"
            x1="${sx(x)}"
            x2="${sx(x)}"
            y1="${sy(ymin)}"
            y2="${sy(ymin) + 7}">
          </line>

          <text
            class="bvn-label"
            x="${sx(x)}"
            y="${sy(ymin) + 28}"
            text-anchor="middle">
            ${x}
          </text>
        `).join("");

      const yTicks =
        ticks.map(y => `
          <line
            class="bvn-tick"
            x1="${sx(xmin) - 7}"
            x2="${sx(xmin)}"
            y1="${sy(y)}"
            y2="${sy(y)}">
          </line>

          <text
            class="bvn-label"
            x="${sx(xmin) - 12}"
            y="${sy(y) + 6}"
            text-anchor="end">
            ${y}
          </text>
        `).join("");

      svg.innerHTML = `
        <line
          class="bvn-axis"
          x1="${sx(xmin)}"
          x2="${sx(xmax)}"
          y1="${sy(ymin)}"
          y2="${sy(ymin)}">
        </line>

        <line
          class="bvn-axis"
          x1="${sx(xmin)}"
          x2="${sx(xmin)}"
          y1="${sy(ymin)}"
          y2="${sy(ymax)}">
        </line>

        ${xTicks}
        ${yTicks}

        ${pointSVG}
        ${contourSVG}
      `;
    }

    draw();
  }

  function conditionalNormalDemo(selector, options = {}) {
    const root = document.querySelector(selector);
    if (!root) return;

    const jointSvg = root.querySelector(".bvn-conditional-joint");
    const condSvg = root.querySelector(".bvn-conditional-density");

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

    function randomNormal() {
      let u = 0;
      let v = 0;

      while (u === 0) u = Math.random();
      while (v === 0) v = Math.random();

      return Math.sqrt(-2 * Math.log(u)) *
        Math.cos(2 * Math.PI * v);
    }

    const nPoints = options.nPoints ?? 220;

    const basePoints = Array.from(
      { length: nPoints },
      () => [randomNormal(), randomNormal()]
    );

    function draw() {
      const mu1 = value("mu1", 0);
      const mu2 = value("mu2", 0);

      const sigma1 = value("sigma1", 1);
      const sigma2 = value("sigma2", 1);

      const rho = value("rho", 0);
      const y0 = value("y0", 0);

      drawJoint(mu1, mu2, sigma1, sigma2, rho, y0);
      drawConditional(mu1, mu2, sigma1, sigma2, rho, y0);
    }

    function drawJoint(mu1, mu2, sigma1, sigma2, rho, y0) {
      const width = 600;
      const height = 600;

      const margin = {
        left: 55,
        right: 25,
        top: 25,
        bottom: 55
      };

      jointSvg.setAttribute(
        "viewBox",
        `0 0 ${width} ${height}`
      );

      const xmin = options.xmin ?? -5;
      const xmax = options.xmax ?? 5;
      const ymin = options.ymin ?? -5;
      const ymax = options.ymax ?? 5;

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
        (y - ymin) / (ymax - ymin) * plotHeight;

      const transformedPoints = basePoints.map(([z1, z2]) => {
        const x =
          mu1 +
          sigma1 * z1;

        const y =
          mu2 +
          sigma2 *
            (
              rho * z1 +
              Math.sqrt(1 - rho * rho) * z2
            );

        return [x, y];
      });

      const a = sigma1 * sigma1;
      const b = rho * sigma1 * sigma2;
      const d = sigma2 * sigma2;

      const trace = a + d;

      const rootTerm =
        Math.sqrt(
          (a - d) * (a - d) +
          4 * b * b
        );

      const lambda1 =
        (trace + rootTerm) / 2;

      const lambda2 =
        (trace - rootTerm) / 2;

      const angle =
        0.5 * Math.atan2(
          2 * b,
          a - d
        );

      function ellipsePoints(level) {
        const r1 =
          level * Math.sqrt(lambda1);

        const r2 =
          level * Math.sqrt(lambda2);

        const cosA = Math.cos(angle);
        const sinA = Math.sin(angle);

        const n = 160;

        return Array.from(
          { length: n + 1 },
          (_, i) => {
            const t =
              2 * Math.PI * i / n;

            const u =
              r1 * Math.cos(t);

            const v =
              r2 * Math.sin(t);

            const x =
              mu1 +
              u * cosA -
              v * sinA;

            const y =
              mu2 +
              u * sinA +
              v * cosA;

            return [sx(x), sy(y)];
          }
        );
      }

      const contourSVG =
        [1, 2, 3].map(level => `
          <path
            class="bvn-contour"
            d="${pathFromPoints(ellipsePoints(level))}"
            opacity="${1 - 0.18 * (level - 1)}">
          </path>
        `).join("");

      const pointSVG =
        transformedPoints.map(([x, y]) => `
          <circle
            class="bvn-point"
            cx="${sx(x)}"
            cy="${sy(y)}"
            r="3">
          </circle>
        `).join("");

      const ticks =
        options.ticks ?? [-4, -2, 0, 2, 4];

      const xTicks =
        ticks.map(x => `
          <line
            class="bvn-tick"
            x1="${sx(x)}"
            x2="${sx(x)}"
            y1="${sy(ymin)}"
            y2="${sy(ymin) + 7}">
          </line>

          <text
            class="bvn-label"
            x="${sx(x)}"
            y="${sy(ymin) + 28}"
            text-anchor="middle">
            ${x}
          </text>
        `).join("");

      const yTicks =
        ticks.map(y => `
          <line
            class="bvn-tick"
            x1="${sx(xmin) - 7}"
            x2="${sx(xmin)}"
            y1="${sy(y)}"
            y2="${sy(y)}">
          </line>

          <text
            class="bvn-label"
            x="${sx(xmin) - 12}"
            y="${sy(y) + 6}"
            text-anchor="end">
            ${y}
          </text>
        `).join("");

      jointSvg.innerHTML = `
        <line
          class="bvn-axis"
          x1="${sx(xmin)}"
          x2="${sx(xmax)}"
          y1="${sy(ymin)}"
          y2="${sy(ymin)}">
        </line>

        <line
          class="bvn-axis"
          x1="${sx(xmin)}"
          x2="${sx(xmin)}"
          y1="${sy(ymin)}"
          y2="${sy(ymax)}">
        </line>

        ${xTicks}
        ${yTicks}

        ${pointSVG}
        ${contourSVG}

        <line
          class="bvn-condition-line"
          x1="${sx(xmin)}"
          x2="${sx(xmax)}"
          y1="${sy(y0)}"
          y2="${sy(y0)}">
        </line>
      `;
    }

    function drawConditional(
      mu1,
      mu2,
      sigma1,
      sigma2,
      rho,
      y0
    ) {
      const width = 600;
      const height = 600;

      const margin = {
        left: 55,
        right: 25,
        top: 25,
        bottom: 55
      };

      condSvg.setAttribute(
        "viewBox",
        `0 0 ${width} ${height}`
      );

      const conditionalMean =
        mu1 +
        rho *
        sigma1 / sigma2 *
        (y0 - mu2);

      const conditionalSd =
        sigma1 *
        Math.sqrt(1 - rho * rho);

      const xmin = options.xmin ?? -5;
      const xmax = options.xmax ?? 5;

      const xs = linspace(xmin, xmax, 300);

      const ys =
        xs.map(x =>
          dnorm(
            x,
            conditionalMean,
            conditionalSd
          )
        );

      const ymax =
        Math.max(...ys) * 1.15;

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

      const points =
        xs.map((x, i) => [
          sx(x),
          sy(ys[i])
        ]);

      const areaPath =
        `M${sx(xs[0])},${sy(0)} ` +
        pathFromPoints(points).replace(/^M/, "L") +
        ` L${sx(xs.at(-1))},${sy(0)} Z`;

      const ticks =
        options.ticks ?? [-4, -2, 0, 2, 4];

      const tickSVG =
        ticks.map(x => `
          <line
            class="bvn-tick"
            x1="${sx(x)}"
            x2="${sx(x)}"
            y1="${sy(0)}"
            y2="${sy(0) + 7}">
          </line>

          <text
            class="bvn-label"
            x="${sx(x)}"
            y="${sy(0) + 28}"
            text-anchor="middle">
            ${x}
          </text>
        `).join("");

      condSvg.innerHTML = `
        <text
          class="bvn-label"
          x="${width / 2}"
          y="24"
          text-anchor="middle">
          X | Y = ${y0.toFixed(1)}
        </text>

        <line
          class="bvn-axis"
          x1="${sx(xmin)}"
          x2="${sx(xmax)}"
          y1="${sy(0)}"
          y2="${sy(0)}">
        </line>

        ${tickSVG}

        <path
          class="bvn-conditional-area"
          d="${areaPath}">
        </path>

        <path
          class="bvn-conditional-curve"
          d="${pathFromPoints(points)}">
        </path>

        <line
          class="bvn-condition-line"
          x1="${sx(conditionalMean)}"
          x2="${sx(conditionalMean)}"
          y1="${sy(0)}"
          y2="${sy(dnorm(
            conditionalMean,
            conditionalMean,
            conditionalSd
          ))}">
        </line>

        <text
          class="bvn-label"
          x="${width / 2}"
          y="${height - 10}"
          text-anchor="middle">
          μ = ${conditionalMean.toFixed(2)},
          σ = ${conditionalSd.toFixed(2)}
        </text>
      `;
    }

    draw();
  }

  /* ============================================================
   SHASH central-limit-theorem demo
   ============================================================ */

function cltShashDemo(selector, options = {}) {
  const root = document.querySelector(selector);
  if (!root) return;

  const populationSvg =
    root.querySelector(".clt-population");

  const replicateSvgs = [
    root.querySelector(".clt-replicate-1"),
    root.querySelector(".clt-replicate-2"),
    root.querySelector(".clt-replicate-3")
  ];

  const samplingSvg =
    root.querySelector(".clt-sampling");

  const controls = {};

  root.querySelectorAll("[data-param]").forEach(input => {
    controls[input.dataset.param] = input;

    const output = root.querySelector(
      `[data-value="${input.dataset.param}"]`
    );

    const updateLabel = () => {
      if (!output) return;

      const value = Number(input.value);

      if (
        input.dataset.param === "n" ||
        input.dataset.param === "reps"
      ) {
        output.textContent =
          Math.round(value).toString();
      } else {
        output.textContent =
          value.toFixed(1);
      }
    };

    updateLabel();

    input.addEventListener("input", () => {
      updateLabel();
      scheduleDraw();
    });
  });

  function value(name, fallback) {
    return controls[name]
      ? Number(controls[name].value)
      : fallback;
  }


  /* ----------------------------------------------------------
     Raw SHASH machinery

     Positive skew produces positive/right skew.

     U ~ N(0,1)

     Y = sinh((asinh(U) + skew) / tail)
     ---------------------------------------------------------- */

  function rawShashDraw(skew, tail) {
    const u =
      jStat.normal.sample(0, 1);

    return Math.sinh(
      (Math.asinh(u) + skew) / tail
    );
  }


  function rawShashDensity(y, skew, tail) {
    const u =
      Math.sinh(
        tail * Math.asinh(y) - skew
      );

    const jacobian =
      tail *
      Math.sqrt(1 + u * u) /
      Math.sqrt(1 + y * y);

    return (
      Math.exp(-0.5 * u * u) /
      Math.sqrt(2 * Math.PI)
    ) * jacobian;
  }


  /*
    The ordinary SHASH location and scale are not generally its
    mean and SD.

    We numerically calculate E(Y) and SD(Y), cache the result,
    then standardize Y so that the user-facing controls really
    correspond to population mean and SD.
  */

  const momentCache = new Map();

  function rawShashMoments(skew, tail) {
    const key =
      `${skew.toFixed(6)}|${tail.toFixed(6)}`;

    if (momentCache.has(key)) {
      return momentCache.get(key);
    }

    const lo = -8;
    const hi = 8;
    const m = 3001;
    const h = (hi - lo) / (m - 1);

    let total = 0;
    let first = 0;
    let second = 0;

    for (let i = 0; i < m; i++) {
      const u = lo + i * h;

      const weight =
        Math.exp(-0.5 * u * u) /
        Math.sqrt(2 * Math.PI);

      const y =
        Math.sinh(
          (Math.asinh(u) + skew) / tail
        );

      const trapezoidWeight =
        (i === 0 || i === m - 1)
          ? 0.5
          : 1;

      total +=
        trapezoidWeight * weight;

      first +=
        trapezoidWeight * weight * y;

      second +=
        trapezoidWeight * weight * y * y;
    }

    total *= h;
    first *= h;
    second *= h;

    const rawMean =
      first / total;

    const rawVariance =
      second / total -
      rawMean * rawMean;

    const result = {
      mean: rawMean,
      sd: Math.sqrt(rawVariance)
    };

    momentCache.set(key, result);

    return result;
  }


  function rshash(
    n,
    mean = 0,
    sd = 1,
    skew = 0,
    tail = 1
  ) {
    const moments =
      rawShashMoments(skew, tail);

    return Array.from(
      { length: n },
      () => {
        const y =
          rawShashDraw(skew, tail);

        return (
          mean +
          sd *
          (y - moments.mean) /
          moments.sd
        );
      }
    );
  }


  function dshashStandardized(
    x,
    mean,
    sd,
    skew,
    tail
  ) {
    const moments =
      rawShashMoments(skew, tail);

    const y =
      moments.mean +
      moments.sd *
      (x - mean) / sd;

    return (
      rawShashDensity(y, skew, tail) *
      moments.sd / sd
    );
  }


  /* ----------------------------------------------------------
     Helpers
     ---------------------------------------------------------- */

  function meanOf(x) {
    return (
      x.reduce((a, b) => a + b, 0) /
      x.length
    );
  }


  function histogram(values, bins, xmin, xmax) {
    const width =
      (xmax - xmin) / bins;

    const counts =
      Array(bins).fill(0);

    for (const x of values) {
      const index =
        Math.floor(
          (x - xmin) / (xmax - xmin) * bins
        );

      if (index >= 0 && index < bins) {
        counts[index]++;
      } else if (x === xmax) {
        counts[bins - 1]++;
      }
    }

    return counts.map((count, i) => ({
      x0: xmin + i * width,
      x1: xmin + (i + 1) * width,
      count
    }));
  }


  function normalDensity(x, mean, sd) {
    return jStat.normal.pdf(x, mean, sd);
  }


  /* ----------------------------------------------------------
     Small plot: density
     ---------------------------------------------------------- */

  function drawDensity(
    svg,
    title,
    density,
    xmin,
    xmax
  ) {
    const width = 300;
    const height = 190;

    const margin = {
      left: 15,
      right: 8,
      top: 28,
      bottom: 20
    };

    svg.setAttribute(
      "viewBox",
      `0 0 ${width} ${height}`
    );

    const xs =
      linspace(xmin, xmax, 250);

    const ys =
      xs.map(density);

    const ymax =
      Math.max(...ys) * 1.08;

    const plotWidth =
      width - margin.left - margin.right;

    const plotHeight =
      height - margin.top - margin.bottom;

    const sx = x =>
      margin.left +
      (x - xmin) /
      (xmax - xmin) *
      plotWidth;

    const sy = y =>
      margin.top +
      plotHeight -
      y / ymax * plotHeight;

    const points =
      xs.map((x, i) => [
        sx(x),
        sy(ys[i])
      ]);

    svg.innerHTML = `
      <text
        class="clt-title"
        x="${width / 2}"
        y="18">
        ${title}
      </text>

      <line
        class="clt-axis"
        x1="${sx(xmin)}"
        x2="${sx(xmax)}"
        y1="${sy(0)}"
        y2="${sy(0)}">
      </line>

      <path
        class="clt-density"
        d="${pathFromPoints(points)}">
      </path>
    `;
  }


  /* ----------------------------------------------------------
     Small plot: one sample histogram
     ---------------------------------------------------------- */

  function drawSmallHistogram(
    svg,
    title,
    values,
    xmin,
    xmax
  ) {
    const width = 300;
    const height = 190;

    const margin = {
      left: 15,
      right: 8,
      top: 28,
      bottom: 20
    };

    svg.setAttribute(
      "viewBox",
      `0 0 ${width} ${height}`
    );

    const bins =
      Math.max(
        4,
        Math.min(
          15,
          Math.round(Math.sqrt(values.length))
        )
      );

    const hist =
      histogram(
        values,
        bins,
        xmin,
        xmax
      );

    const ymax =
      Math.max(
        1,
        ...hist.map(d => d.count)
      );

    const plotWidth =
      width - margin.left - margin.right;

    const plotHeight =
      height - margin.top - margin.bottom;

    const sx = x =>
      margin.left +
      (x - xmin) /
      (xmax - xmin) *
      plotWidth;

    const sy = y =>
      margin.top +
      plotHeight -
      y / ymax * plotHeight;

    const bars =
      hist.map(d => `
        <rect
          class="clt-histogram"
          x="${sx(d.x0)}"
          y="${sy(d.count)}"
          width="${Math.max(
            0,
            sx(d.x1) - sx(d.x0) - 1
          )}"
          height="${
            sy(0) - sy(d.count)
          }">
        </rect>
      `).join("");

    svg.innerHTML = `
      <text
        class="clt-title"
        x="${width / 2}"
        y="18">
        ${title}
      </text>

      <line
        class="clt-axis"
        x1="${sx(xmin)}"
        x2="${sx(xmax)}"
        y1="${sy(0)}"
        y2="${sy(0)}">
      </line>

      ${bars}
    `;
  }


  /* ----------------------------------------------------------
     Large bottom plot: sampling distribution
     ---------------------------------------------------------- */

  function drawSamplingDistribution(
    values,
    populationMean,
    populationSd,
    n
  ) {
    const width = 900;
    const height = 400;

    const margin = {
      left: 45,
      right: 15,
      top: 35,
      bottom: 40
    };

    samplingSvg.setAttribute(
      "viewBox",
      `0 0 ${width} ${height}`
    );

    const theoreticalSE =
      populationSd / Math.sqrt(n);

    /*
      Use an adaptive x-axis so the shape of the sampling
      distribution remains visible as n increases.
    */
    const halfRange =
      Math.max(
        4.5 * theoreticalSE,
        0.35 * populationSd
      );

    const xmin =
      populationMean - halfRange;

    const xmax =
      populationMean + halfRange;

    const bins =
      Math.max(
        15,
        Math.min(
          45,
          Math.round(Math.sqrt(values.length))
        )
      );

    const hist =
      histogram(
        values,
        bins,
        xmin,
        xmax
      );

    const binWidth =
      (xmax - xmin) / bins;

    /*
      Histogram density rather than raw count, so the normal
      reference curve can be drawn on the same scale.
    */
    const histDensity =
      hist.map(d => ({
        ...d,
        density:
          d.count /
          (values.length * binWidth)
      }));

    const xs =
      linspace(xmin, xmax, 300);

    const normalYs =
      xs.map(x =>
        normalDensity(
          x,
          populationMean,
          theoreticalSE
        )
      );

    const ymax =
      Math.max(
        ...histDensity.map(d => d.density),
        ...normalYs
      ) * 1.1;

    const plotWidth =
      width - margin.left - margin.right;

    const plotHeight =
      height - margin.top - margin.bottom;

    const sx = x =>
      margin.left +
      (x - xmin) /
      (xmax - xmin) *
      plotWidth;

    const sy = y =>
      margin.top +
      plotHeight -
      y / ymax * plotHeight;

    const bars =
      histDensity.map(d => `
        <rect
          class="clt-histogram"
          x="${sx(d.x0)}"
          y="${sy(d.density)}"
          width="${Math.max(
            0,
            sx(d.x1) - sx(d.x0) - 1
          )}"
          height="${
            sy(0) - sy(d.density)
          }">
        </rect>
      `).join("");

    const normalPoints =
      xs.map((x, i) => [
        sx(x),
        sy(normalYs[i])
      ]);

    samplingSvg.innerHTML = `
      <text
        class="clt-title"
        x="${width / 2}"
        y="20">
        Sampling distribution of the sample mean
      </text>

      <line
        class="clt-axis"
        x1="${sx(xmin)}"
        x2="${sx(xmax)}"
        y1="${sy(0)}"
        y2="${sy(0)}">
      </line>

      ${bars}

      <path
        class="clt-normal-reference"
        d="${pathFromPoints(normalPoints)}">
      </path>

      <text
        class="clt-summary"
        x="${width / 2}"
        y="${height - 8}">
        Normal reference:
        μ = ${populationMean.toFixed(2)},
        SE = ${theoreticalSE.toFixed(2)}
      </text>
    `;
  }


  /* ----------------------------------------------------------
     Simulation
     ---------------------------------------------------------- */

  function simulate() {
    const populationMean =
      value("mean", 0);

    const populationSd =
      value("sd", 1);

    const skew =
      value("skew", 0);

    const tail =
      value("tail", 1);

    const n =
      Math.max(
        1,
        Math.round(value("n", 30))
      );

    const reps =
      Math.max(
        3,
        Math.round(value("reps", 2000))
      );

    const populationXMin =
      populationMean -
      5 * populationSd;

    const populationXMax =
      populationMean +
      5 * populationSd;


    /*
      Draw population density.
    */

    drawDensity(
      populationSvg,
      "Population",
      x =>
        dshashStandardized(
          x,
          populationMean,
          populationSd,
          skew,
          tail
        ),
      populationXMin,
      populationXMax
    );


    /*
      Simulate repetitions.

      We only retain the first three full samples.
      For all later repetitions we only retain the mean.
      This keeps memory use tiny.
    */

    const firstSamples = [];
    const sampleMeans = [];

    for (let r = 0; r < reps; r++) {
      const sample =
        rshash(
          n,
          populationMean,
          populationSd,
          skew,
          tail
        );

      if (r < 3) {
        firstSamples.push(sample);
      }

      sampleMeans.push(
        meanOf(sample)
      );
    }


    /*
      First three observed samples.
    */

    for (let i = 0; i < 3; i++) {
      drawSmallHistogram(
        replicateSvgs[i],
        `Sample ${i + 1}`,
        firstSamples[i],
        populationXMin,
        populationXMax
      );
    }


    /*
      Sampling distribution.
    */

    drawSamplingDistribution(
      sampleMeans,
      populationMean,
      populationSd,
      n
    );
  }


  /*
    A little debouncing makes dragging n/repetitions smooth
    rather than running a full simulation for every pixel.
  */

  let timer = null;

  function scheduleDraw() {
    clearTimeout(timer);

    timer =
      setTimeout(simulate, 60);
  }

  simulate();
}

window.cltShashDemo = cltShashDemo;

  window.conditionalNormalDemo = conditionalNormalDemo;

  window.bivariateNormalDemo = bivariateNormalDemo;

  window.shashDemo = shashDemo;
  window.dshash = dshash;

  window.normalDemo = normalDemo;
  window.dnorm = dnorm;
  window.pnorm = pnorm;
  window.qnorm = qnorm;

})();