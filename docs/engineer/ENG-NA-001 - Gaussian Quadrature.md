# Gaussian Quadrature

## Bibliography

### Reference 1

Title: Numerical Analysis

Authors: Richard L. Burden and J. Douglas Faires

## Equations

Equation Polynomial of Lagrange:

$$
L_i(x) = \prod_{\substack{j = 1 \\ j \neq i}}^{n} \frac{x - x_j}{x_i - x_j}
$$

Coefficient $c_i$:

$$
\int_{-1}^1 P(x) \; dx = \sum_{i=1}^{n} c_i P(x_i)
$$

Range modify $t \longleftrightarrow x$:

Write $t$ in function of $x$

$$
t = \frac{2x - a - b}{b - a} \longleftrightarrow x = \frac{(b - a)t + (b + a)}{2}
$$

New integration

$$
\int_a^b f(x) \; dx = \int_{-1}^{1} f\left(\frac{(b - a)t + (b + a)}{2}\right) \frac{b - a}{2} \; dt
$$
