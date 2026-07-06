# Math

## Vertex setup

Each vertex must be converted from clip space to screen space. The vertex
shader produces homogenous coordinates $(x_{clip}, y_{clip}, z_{clip}, w)$
We compute the reciprocal of w once per vertex, and then apply that to the
other variables to perspective correct their locations:

```math
x_{screen} = x_{clip} \cdot w^{-1}\\
y_{screen} = y_{clip} \cdot w^{-1}\\
z_{screen} = z_{clip} \cdot w^{-1}
```

## Triangle setup, rasterization, pixel processing

We compute pixel coverage using the classic Pineda edge equation algorithm[^1]
as follows. For each of three edges in the triangle in screen space, we
assume $(X, Y)$ represents the start of the edge (in winding order)
and $(X + dX, Y + dY)$ is the end point. From there, the edge equation
for any arbitrary point on the plane $(x,y)$ using the cross product is:

```math
E_n(x,y) = (x - X_n)dY_n - (y - Y_n)dX_n
```

If $E_n$ is positive for all three edge functions, then the point is inside
the triangle, otherwise it is outside. The nice thing about these equations
is they can be done completely with integer math, and the edge function can
be updated incrementally with additions and subtractions of $dY_n$ and $dX_n$
as we scan the triangle, making the hardware fairly simple.

As it turns out, these edge equations are also barycentric coordinates: each
one represents how close a point is to one of the vertices[^2]. As such, we can
reuse this math for parameter interpolation. In order for this to work
properly, we first need to normalize them. At any point, the three edges
will always sum to the same value, which is the 2 times the area of the triangle:

```math
\lambda_n = \frac{E_n}{E_0 + E_1 + E_2} \\
```

The normalized values can then be used to trivially interpolate a parameter P
across the triangle (where $P_n$ represents the value of a parameter at vertex
n, and $p_{(\lambda_0, \lambda_1, \lambda_2)}$ represents an arbitrary point
within the triangle).

```math
p_{(\lambda_0, \lambda_1, \lambda_2)} = P_0\lambda_0 + P_1\lambda_1 + P_2\lambda_2
```

This allows us to linearly interpolate any value across the triangle in screen
space, but we need to do this in a perspective correct manner, which is
described in [^3]. The full formula for perspective correct interpolated value is
(note, we use W here, which is the pre-divide clip space depth)

```math
p'_{(\lambda_0, \lambda_1, \lambda_2)} = \frac{\lambda_0 P_0 W_0^{-1} + \lambda_1 P_1 W_1^{-1} + \lambda_2 P_2 W_2^{-1}}{\lambda_0 W_0^{-1} + \lambda_1 W_1^{-1} + \lambda_2 W_2^{-1}}
```

But, when we have multiple parameters per triangle, we can optimize this by
combining the two concepts above, using
*perspective correct barycentric coordinates*. We first interpolate the reciprocal of W:

```math
\frac{1}{W_{pixel}} = \lambda_0\frac{1}{W_0} + \lambda_1\frac{1}{W_1} + \lambda_2\frac{1}{W_2}
```

We then need to execute one reciprocal per pixel to get the perspective correct
W. We can then calculate the perspective correct barycentrics:

```math
\lambda_n' = \lambda_n\frac{W_{pixel}}{W_n}
```

Which can be used to compute any specific parameter at a point in the plane:

```math
p_{pixel} = \lambda_0'P_0 + \lambda_1'P_1 + \lambda_2'P_2
```

We can further optimize the original linear interpolation. Before talking about
how this can be applied to perspective correct rendering, let's focus on the
linear form. Since the three normalized $\lambda$ values add up to 1, we can simplify the calculation, given:

```math
\lambda_0 + \lambda_1 + \lambda_2  = 1 \\
```

We can substitute:

```math
\lambda_0 = 1 - \lambda_1 - \lambda_2 \\
q_{(\lambda_0, \lambda_1, \lambda_2)} = Q_0(1 - \lambda_1 - \lambda_2) + \lambda_1Q_1 + \lambda_2Q_2 \rightarrow \\
q_{(\lambda_0, \lambda_1, \lambda_2)} = Q_0 - Q_0\lambda_1 - Q_0\lambda_2 + \lambda_1Q_1 + \lambda_2Q_2 \rightarrow \\
q_{(\lambda_0, \lambda_1, \lambda_2)} = Q_0 + \lambda_1(Q_1 - Q_0) + \lambda_2(Q_2 - Q_0)
```

We precompute two values at triangle setup time:

```math
dQ_1 = Q_1 - Q_0 \\
dQ_2 = Q_2 - Q_0
```

Resulting in this formula:

```math
q_{(\lambda_1, \lambda_2)} = Q_0 + \lambda_1 dQ_1 + \lambda_2 dQ_2
```

In a perspective-correct hardware pipeline, we apply this subtraction optimization to the screen-space interpolation of both the depth reciprocal ($\frac{1}{W}$) and the attributes.

[^1]: Pineda, Juan. "A parallel algorithm for polygon rasterization." Proceedings of the 15th annual conference on Computer graphics and interactive techniques. 1988.
[^2]: Brown, Russell A. "Barycentric coordinates as interpolants." arXiv preprint arXiv:1308.1279 (2013).
[^3]: Low, Kok-Lim. "Perspective-correct interpolation." Technical writing, Department of Computer Science, University of North Carolina at Chapel Hill (2002).
