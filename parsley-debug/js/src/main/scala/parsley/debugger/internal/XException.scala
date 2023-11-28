package parsley.debugger.internal

import scala.scalajs.js

// Cross platform wrapper around UnsupportedOperationException.
private [parsley] class XUnsupportedOperationException(msg: String) {
    def except: Throwable = js.JavaScriptException("UnsupportedOperationException: " + msg)
}

// Cross platform wrapper around IllegalStateException.
private [parsley] class XIllegalStateException(msg: String) {
    def except: Throwable = js.JavaScriptException("IllegalStateException: " + msg)
}
