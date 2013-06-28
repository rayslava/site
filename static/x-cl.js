/* execute all script tags with type of x-common-lisp */
window.onload = (function () {
  var scripts = document.scripts;

  for (var i = 0; i < scripts.length; ++i) {
    var script = scripts[i];

    /* TODO: what about errors? */
    if (script.type == "text/x-common-lisp") {
      lisp.evalString(script.text);
    }
  }
});

// document.addEventListener( "DOMContentLoaded", parselisp, false )
