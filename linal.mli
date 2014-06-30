type matrix
type vector

val dot : vector -> vector -> float
val vadd : vector -> vector -> vector
val vminus : vector -> vector -> vector

val identity : float -> float -> matrix

val determinant : matrix -> float

val transpose : matrix -> matrix
val invert : matrix -> matrix

val mtimes : matrix -> matrix -> matrix
val mplus : matrix -> matrix -> matrix
val mminus : matrix -> matrix -> matrix

val mcat : matrix -> matrix -> matrix
