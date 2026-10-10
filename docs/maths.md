# Maths

## Triangle setup, rasterization, pixel processing

We compute pixel coverage using the classic Pineda rasterization algorithm[^1].  For each of three edges in the triangle in screen space, we
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
as we scan the triangle, making the hardware trivial.

As it turns out, these edge equations are also barycentric coordinates: each
one represents how close a point is to one of the vertices[^2]. You may
recognize this as a *determinant*. This is elegant because we can reuse these
values for parameter interpolation. In order for this to work  properly, we
first need to normalize them so that all three always add up to 1.0. When we
first compute the edge equation values using the value above, they add up to
two times the area of the triangle, so we can divide by that to normalize them.

```math
\lambda_n = \frac{E_n}{E_0 + E_1 + E_2}
```

The normalized values can then be used to interpolate a parameter P
across the triangle (where $P_n$ represents the value of a parameter at vertex
n, and $p_{(\lambda_0, \lambda_1, \lambda_2)}$ represents an arbitrary point
within the triangle).

```math
p_{(\lambda_0, \lambda_1, \lambda_2)} = P_0\lambda_0 + P_1\lambda_1 + P_2\lambda_2
```

This allows us to linearly interpolate any value across the triangle in screen
space, but we need to do this in a *perspective correct* manner (meaning things get closer together as they are farther from the viewer), which is
described in [^3]. The full formula for perspective correct interpolated value is
(note, we use W here, which is the pre-divide clip space depth)

```math
p'_{(\lambda_0, \lambda_1, \lambda_2)} = \frac{\lambda_0 P_0 W_0^{-1} + \lambda_1 P_1 W_1^{-1} + \lambda_2 P_2 W_2^{-1}}{\lambda_0 W_0^{-1} + \lambda_1 W_1^{-1} + \lambda_2 W_2^{-1}}
```

But, when we have multiple parameters per triangle, we can optimize this by
combining the two concepts above, using
*perspective correct barycentric coordinates*. These are advantageous
because they reduce the computations for each parameter in both setup and
interpolation. We first interpolate the reciprocal of W:

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
\lambda_0 + \lambda_1 + \lambda_2  = 1
```

We can substitute:

```math
\begin{aligned}
\lambda_0 = 1 - \lambda_1 - \lambda_2 \\
q_{(\lambda_0, \lambda_1, \lambda_2)} = Q_0(1 - \lambda_1 - \lambda_2) + \lambda_1Q_1 + \lambda_2Q_2 \rightarrow \\
q_{(\lambda_0, \lambda_1, \lambda_2)} = Q_0 - Q_0\lambda_1 - Q_0\lambda_2 + \lambda_1Q_1 + \lambda_2Q_2 \rightarrow \\
q_{(\lambda_0, \lambda_1, \lambda_2)} = Q_0 + \lambda_1(Q_1 - Q_0) + \lambda_2(Q_2 - Q_0)
\end{aligned}
```

We precompute two values at triangle setup time:

```math
\begin{aligned}
dQ_1 = Q_1 - Q_0 \\\\
dQ_2 = Q_2 - Q_0
\end{aligned}
```

Resulting in this formula:

```math
q_{(\lambda_1, \lambda_2)} = Q_0 + \lambda_1 dQ_1 + \lambda_2 dQ_2
```

In a perspective-correct hardware pipeline, we apply this subtraction optimization to the screen-space interpolation of both the depth reciprocal ($\frac{1}{W}$) and the attributes.

Putting this all together:

**Triangle Setup**

Set up depth interpolation coefficients

```math
    invW0 = \frac{1.0}{z_0} \\
    invW1 = \frac{1.0}{z_1} \\
    invW2 = \frac{1.0}{z_2} \\
    invdW1 = invW1 - invW0 \\
    invdW2 = invW2 - invW1 \\
```

Set up rasterizer coefficients

```math
    dx_0 = x_1 - x_0 \\
    dy_0 = y_1 - y_0 \\
    edge_0 = (x_{start} - x_0) * dx_0 +  (y_{start} - y_0) * dy_0 \\

    dx_1 = x_2 - x_1 \\
    dy_1 = y_2 - y_1 \\
    edge_1 = (x_{start} - x_1) * dx_1+  (y_{start} - y_1) * dy_1 \\

    dx_2 = x_0 - x_2 \\
    dy_2 = y_0 - y_2 \\
    edge_2 = (x_{start} - x_2) * dx_2 +  (y_{start} - y_2) * dy_2
```

Normalize

```math
    normFactor = \frac{1.0}{edge_0 + edge_1 + edge_2} \\
    dx_n *= normFactor \\
    dy_n *= normFactor \\
    edge_n *= normFactor
```

For each parameter

```math
    P_0 = p_0 \\
    dP_1 = p_1 - p_0 \\
    dP_2 = p_2 - p_0
```

**Per pixel**

First, we compute the inverse depth and depth at each pixel

```math
    invW_{pixel} = invW_0 + invW_1 * \lambda_1 + invW_2 * \lambda_2 \\
    w_{pixel} = \frac{1.0}{invW_{pixel}}
```

Then compute the perspective correct barycentric coordinates:

```math
    \lambda_{pixel}'= \lambda_{pixel} \cdot w_{pixel} \cdot invW_n
```

[^1]: Pineda, Juan. "A parallel algorithm for polygon rasterization." Proceedings of the 15th annual conference on Computer graphics and interactive techniques. 1988.
[^2]: Brown, Russell A. "Barycentric coordinates as interpolants." arXiv preprint arXiv:1308.1279 (2013).
[^3]: Low, Kok-Lim. "Perspective-correct interpolation." Technical writing, Department of Computer Science, University of North Carolina at Chapel Hill (2002).
