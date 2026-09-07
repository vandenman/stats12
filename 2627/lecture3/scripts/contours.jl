using GLMakie
using LinearAlgebra

GLMakie.activate!()

# ------------------------------------------------------------
# Bivariate normal
# ------------------------------------------------------------

μ = [0.0, 0.0]

Σ = [
     1.0  -0.5
    -0.5   1.5
]

Σinv = inv(Σ)
normalizer = 1 / (2π * sqrt(det(Σ)))

function bvn_pdf(x, y)
    d = [x, y] - μ
    normalizer * exp(-0.5 * dot(d, Σinv * d))
end

xs = range(-3.5, 3.5, length = 180)
ys = range(-4.0, 4.0, length = 180)

Z = [bvn_pdf(x, y) for x in xs, y in ys]

zmax = maximum(Z)
zfloor = -0.08 * zmax


# ------------------------------------------------------------
# Figure
# ------------------------------------------------------------

fig = Figure(
    size = (1100, 750),
    fontsize = 28
)

ax = Axis3(
    fig[1, 1];

    xlabel = "X",
    ylabel = "Y",
    zlabel = "Density",

    zlabeloffset = 70,

    aspect = (1, 1, 0.7),
    viewmode = :fit,

    azimuth = -0.65π,
    elevation = 0.20π,
    perspectiveness = 0.7,

    limits = (
        minimum(xs), maximum(xs),
        minimum(ys), maximum(ys),
        zfloor, 1.05zmax
    )
)


# ------------------------------------------------------------
# Density surface
# ------------------------------------------------------------

surf = surface!(
    ax,
    xs,
    ys,
    Z;

    color = Z,
    colormap = :viridis,

    transparency = true,
    alpha = 0.95
)


# ------------------------------------------------------------
# Equal-density contours
#
# For a multivariate normal, contours correspond to constant
# Mahalanobis distance:
#
#   (x - μ)' Σ⁻¹ (x - μ) = r²
#
# We construct those ellipses explicitly.
# ------------------------------------------------------------

L = cholesky(Symmetric(Σ)).L

θ = range(0, 2π, length = 400)

# Mahalanobis radii of the contour lines
radii = [0.75, 1.25, 1.75, 2.25]

contour_z = Observable[]
contour_heights = Float64[]

for r in radii

    ellipse = [
        μ + L * (r .* [cos(t), sin(t)])
        for t in θ
    ]

    ex = first.(ellipse)
    ey = last.(ellipse)

    # All points on this ellipse have the same density.
    h = normalizer * exp(-0.5r^2)

    zobs = Observable(fill(h, length(θ)))

    lines!(
        ax,
        ex,
        ey,
        zobs;

        color = :black,
        linewidth = 4,
        overdraw = true
    )

    push!(contour_z, zobs)
    push!(contour_heights, h)
end


# ------------------------------------------------------------
# Animation helpers
# ------------------------------------------------------------

function smoothstep(x)
    x = clamp(x, 0, 1)
    x^2 * (3 - 2x)
end

function phase(t, start, stop)
    smoothstep((t - start) / (stop - start))
end


function set_state!(t)

    # --------------------------------------------------------
    # Phase 1:
    # Drop equal-density curves from the surface to the floor.
    # --------------------------------------------------------

    drop = phase(t, 0.15, 0.50)

    for (zobs, h) in zip(contour_z, contour_heights)

        z = (1 - drop) * h + drop * zfloor

        zobs[] = fill(z, length(θ))
    end


    # --------------------------------------------------------
    # Phase 2:
    # Move from perspective 3D view to a top-down,
    # nearly orthographic view.
    # --------------------------------------------------------

    camera = phase(t, 0.45, 0.85)

    initial_elevation = 0.20π
    final_elevation   = 0.499π

    ax.elevation[] =
        (1 - camera) * initial_elevation +
        camera * final_elevation

    ax.perspectiveness[] =
        (1 - camera) * 0.7


    # --------------------------------------------------------
    # Phase 3:
    # Fade the surface, leaving the projected contour lines.
    # --------------------------------------------------------

    fade = phase(t, 0.55, 0.90)

    surf.alpha[] =
        (1 - fade) * 0.95 +
        fade * 0.06
end


# ------------------------------------------------------------
# Save useful stills
# ------------------------------------------------------------

set_state!(0.0)
save("bvn-3d.png", fig)

set_state!(1.0)
save("bvn-contour-view.png", fig)


# ------------------------------------------------------------
# Record animation
# ------------------------------------------------------------

fps = 30
seconds = 6

record(
    fig,
    "bvn-3d-to-contour.mp4",
    range(0, 1, length = fps * seconds);

    framerate = fps,
    compression = 25
) do t

    set_state!(t)
end
