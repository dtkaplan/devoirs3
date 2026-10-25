(() => {
  "use strict";

  const collectorEnabled = document.currentScript.dataset.devoirs3Collector === "true";
  const labelsEnabled = document.currentScript.dataset.devoirs3Labels === "true";
  const stateByWidget = new WeakMap();
  const storagePrefix = "devoirs3:v1:";

  function widgetFor(target) {
    return target.closest('[data-devoirs3-widget]');
  }

  function inputFor(widget) {
    return widget.querySelector("[data-devoirs3-input]");
  }

  function isSelection(widget) {
    return widget.dataset.devoirs3Widget === "selection";
  }

  function selectionInputs(widget) {
    return Array.from(widget.querySelectorAll("[data-devoirs3-option]"));
  }

  function normalizeMathDelimiters(text) {
    return text
      .replace(/(^|[^\\])\$\$([\s\S]+?)\$\$(?!\$)/g, "$1\\[$2\\]")
      .replace(/(^|[^\\])\$([^$\n]+?)\$(?!\$)/g, "$1\\($2\\)");
  }

  function setFeedback(feedback, text) {
    const mathJax = window.MathJax;
    if (mathJax && typeof mathJax.typesetClear === "function") {
      mathJax.typesetClear([feedback]);
    }
    feedback.textContent = normalizeMathDelimiters(text);
    feedback.hidden = !text;
    if (!text || !mathJax || typeof mathJax.typesetPromise !== "function") return;

    const typeset = () => mathJax.typesetPromise([feedback]);
    const ready = mathJax.startup && mathJax.startup.promise
      ? mathJax.startup.promise.then(typeset)
      : typeset();
    if (ready && typeof ready.catch === "function") ready.catch(() => {});
  }

  function storageKey(widget) {
    return storagePrefix + encodeURIComponent(widget.dataset.docid) + ":" +
      encodeURIComponent(widget.dataset.label);
  }

  function createSubmission() {
    const widgets = Array.from(document.querySelectorAll("[data-devoirs3-widget]"));
    const docID = widgets[0]?.dataset.docid || "";
    const responses = [];

    widgets.forEach((widget) => {
      if (isSelection(widget)) {
        const value = selectionInputs(widget)
          .filter((input) => input.checked)
          .map((input) => input.dataset.optionId);
        if (value.length) {
          responses.push({ label: widget.dataset.label, type: "selection", value });
        }
        return;
      }

      const value = inputFor(widget).value.replace(/\r\n?/g, "\n");
      if (!value.length) return;
      const state = stateByWidget.get(widget);
      const activeMs = state.activeSince !== null && !document.hidden
        ? performance.now() - state.activeSince
        : 0;
      const duration = Math.round(state.elapsed + activeMs) / 1000;
      responses.push({ label: widget.dataset.label, type: "text", value, duration });
    });

    return {
      schemaVersion: 1,
      docID,
      collectedAt: new Date().toISOString(),
      responses
    };
  }

  function initializeCollector() {
    if ((!collectorEnabled && !labelsEnabled) || document.querySelector("[data-devoirs3-collector-ui]")) return;

    const toolbar = document.createElement("div");
    toolbar.className = "devoirs3-collector";
    toolbar.dataset.devoirs3CollectorUi = "";

    if (collectorEnabled) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "devoirs3-collector-button";
      button.textContent = "Copy answers";

      const status = document.createElement("span");
      status.className = "devoirs3-collector-status";
      status.setAttribute("role", "status");
      status.setAttribute("aria-live", "polite");

      const fallback = document.createElement("textarea");
      fallback.className = "devoirs3-collector-fallback";
      fallback.setAttribute("aria-label", "Collected answers");
      fallback.readOnly = true;
      fallback.rows = 1;
      fallback.hidden = true;

      button.addEventListener("click", async () => {
        const submission = JSON.stringify(createSubmission());
        try {
          if (!navigator.clipboard || typeof navigator.clipboard.writeText !== "function") {
            throw new Error("Clipboard API unavailable");
          }
          await navigator.clipboard.writeText(submission);
          fallback.hidden = true;
          status.textContent = "Answers copied.";
        } catch (_) {
          fallback.value = submission;
          fallback.hidden = false;
          status.textContent = "Clipboard unavailable. Copy the answers below.";
          fallback.focus();
          fallback.select();
        }
      });

      toolbar.append(button, status, fallback);
    }

    if (labelsEnabled) {
      const labelToggle = document.createElement("label");
      labelToggle.className = "devoirs3-label-toggle";
      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.addEventListener("change", () => {
        document.querySelectorAll("[data-devoirs3-label]").forEach((label) => {
          label.hidden = !checkbox.checked;
        });
      });
      labelToggle.append(checkbox, document.createTextNode("Labels?"));
      toolbar.append(labelToggle);
    }

    document.body.prepend(toolbar);
  }

  function save(widget) {
    if (isSelection(widget)) {
      const selected = selectionInputs(widget)
        .filter((input) => input.checked)
        .map((input) => input.dataset.optionId);
      try {
        if (selected.length) {
          localStorage.setItem(storageKey(widget), JSON.stringify({ value: selected }));
        } else {
          localStorage.removeItem(storageKey(widget));
        }
      } catch (_) {
        // The widget remains usable when browser storage is unavailable.
      }
      return;
    }
    const input = inputFor(widget);
    const state = stateByWidget.get(widget);
    let duration = state.elapsed;
    if (state.activeSince !== null && !document.hidden) {
      duration += performance.now() - state.activeSince;
    }
    try {
      localStorage.setItem(storageKey(widget), JSON.stringify({
        value: input.value,
        durationMs: duration
      }));
    } catch (_) {
      // The widget remains usable when browser storage is unavailable.
    }
  }

  function pause(widget) {
    const state = stateByWidget.get(widget);
    if (state.activeSince === null) return;
    state.elapsed += performance.now() - state.activeSince;
    state.activeSince = null;
    save(widget);
  }

  function resume(widget) {
    const state = stateByWidget.get(widget);
    if (!document.hidden && state.activeSince === null) {
      state.activeSince = performance.now();
    }
  }

  function initialize(root = document) {
    root.querySelectorAll("[data-devoirs3-widget]").forEach((widget) => {
      if (stateByWidget.has(widget)) return;
      if (isSelection(widget)) {
        let saved = null;
        try {
          saved = JSON.parse(localStorage.getItem(storageKey(widget)) || "null");
        } catch (_) {
          saved = null;
        }
        if (saved && Array.isArray(saved.value)) {
          const selected = new Set(saved.value);
          selectionInputs(widget).forEach((input) => {
            input.checked = selected.has(input.dataset.optionId);
          });
        }
        stateByWidget.set(widget, {});
        return;
      }
      const input = inputFor(widget);
      let saved = null;
      try {
        saved = JSON.parse(localStorage.getItem(storageKey(widget)) || "null");
      } catch (_) {
        saved = null;
      }
      const durationMs = saved && Number.isFinite(saved.durationMs)
        ? Math.max(0, saved.durationMs)
        : 0;
      stateByWidget.set(widget, { elapsed: durationMs, activeSince: null });
      if (saved && typeof saved.value === "string") input.value = saved.value;
    });
    if (root === document) initializeCollector();
  }

  function initializeWhenReady() {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", () => initialize(), { once: true });
    } else {
      initialize();
    }
  }

  document.addEventListener("input", (event) => {
    if (!event.target.matches("[data-devoirs3-input]")) return;
    const widget = widgetFor(event.target);
    if (widget) save(widget);
  });

  document.addEventListener("change", (event) => {
    if (!event.target.matches("[data-devoirs3-option]")) return;
    const widget = widgetFor(event.target);
    if (!widget) return;
    save(widget);
    const feedback = widget.querySelector("[data-devoirs3-feedback]");
    if (widget.dataset.feedbackEnabled === "true") {
      const feedbackText = widget.dataset.multiple === "true" && !event.target.checked
        ? ""
        : event.target.dataset.feedback || "";
      setFeedback(feedback, feedbackText);
    }
  });

  document.addEventListener("focusin", (event) => {
    if (!event.target.matches("[data-devoirs3-input]")) return;
    const widget = widgetFor(event.target);
    if (widget) resume(widget);
  });

  document.addEventListener("focusout", (event) => {
    if (!event.target.matches("[data-devoirs3-input]")) return;
    const widget = widgetFor(event.target);
    if (widget) pause(widget);
  });

  document.addEventListener("click", (event) => {
    const selectionButton = event.target.closest("[data-devoirs3-clear-selection]");
    if (selectionButton) {
      const selectionWidget = widgetFor(selectionButton);
      if (!selectionWidget) return;
      selectionInputs(selectionWidget).forEach((input) => {
        input.checked = false;
      });
      const feedback = selectionWidget.querySelector("[data-devoirs3-feedback]");
      setFeedback(feedback, "");
      try {
        localStorage.removeItem(storageKey(selectionWidget));
      } catch (_) {
        // Clearing the visible choices still works without browser storage.
      }
      return;
    }
    const button = event.target.closest("[data-devoirs3-clear]");
    if (!button) return;
    const widget = widgetFor(button);
    if (!widget) return;
    const state = stateByWidget.get(widget);
    state.elapsed = 0;
    state.activeSince = null;
    inputFor(widget).value = "";
    try {
      localStorage.removeItem(storageKey(widget));
    } catch (_) {
      // Clearing the visible field still works without browser storage.
    }
  });

  document.addEventListener("visibilitychange", () => {
    document.querySelectorAll('[data-devoirs3-widget="text"]').forEach((widget) => {
      if (document.hidden) {
        pause(widget);
      } else if (inputFor(widget) === document.activeElement) {
        resume(widget);
      }
    });
  });

  window.addEventListener("pagehide", () => {
    document.querySelectorAll('[data-devoirs3-widget="text"]').forEach(pause);
  });

  initializeWhenReady();
})();
