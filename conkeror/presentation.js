function injectScript (doc, code) {
  var script = doc.createElementNS(XHTML_NS, "script");
  script.textContent = code;
  doc.body.appendChild(script);
}

interactive("pres-prev", "Previous slide in a presentation", function (I) {
  injectScript(I.buffer.document, "Dz.back();")
});

interactive("pres-next", "Next slide in a presentation", function (I) {
  injectScript(I.buffer.document, "Dz.forward();")
});
