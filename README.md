# MatrixFlow

This is a dumb simple naive implementation of _TensorFlow_ like framework for matrices.
I implemented it as a tiny toy project to learn how a back-propagation work.
I tried to keep the code as `Scala`-ish as possible.

## Possible improvements

Those are just two must have ideas (for me to not to forget):

 * the forward pass should be run only once and cached for the backward pass to be efficient.
 * create interface for `Op` such that an explicit function for derivatives of all parameters can be specified
  * might be useful when implementing more sophisticated numerical methods...
 * ...

## Licence [MIT](LICENCE.md)
