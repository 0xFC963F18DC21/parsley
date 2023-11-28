package parsley.debugger.internal

// Cross platform wrapper around UnsupportedOperationException.
private [parsley] class XUnsupportedOperationException(msg: String) {
    def except: Throwable = new UnsupportedOperationException(msg)
}

// Cross platform wrapper around IllegalStateException.
private [parsley] class XIllegalStateException(msg: String) {
    def except: Throwable = new IllegalStateException(msg)
}
