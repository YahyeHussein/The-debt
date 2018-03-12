module Helper

open Error

// All group

// this is quite halpful.
let ($) x f =
    bind f x