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

  window.bivariateNormalDemo = bivariateNormalDemo;

  window.shashDemo = shashDemo;
  window.dshash = dshash;

  window.normalDemo = normalDemo;
  window.dnorm = dnorm;
  window.pnorm = pnorm;
  window.qnorm = qnorm;

})();