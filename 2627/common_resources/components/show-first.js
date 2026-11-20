document.addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll(".svg-stack:not(.hide-first) li:first-child")
    .forEach(li => li.classList.remove("fragment"));
});