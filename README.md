# forncnn

Experimental Fortran binding for the ncnn `c_api`.

## Requirements

- gfortran, or any compiler implementing Fortran 2018's `iso_c_binding` (the
  code uses `c_int8_t` and `error stop`; developed and tested against GCC 16)
- ncnn built **with the C API enabled** (`NCNN_C_API`, on by default)

This build was verified against ncnn `1.0.20261106` configured with
`NCNN_VULKAN=OFF` and everything else on. See the coverage table below for what
a different configuration changes.

## Build

fpm is the intended build system:

```sh
fpm build
fpm test
fpm run --example squeezenet
```

`fpm.toml` links `-lncnn`. If ncnn is installed in a standard location that is
all you need. When it is not — a local build tree, say — point the linker at it
with `FPM_LDFLAGS`. ncnn compiled with clang also needs libc++, and its default
CMake configuration enables OpenMP, so on macOS a local build typically needs:

```sh
export FPM_LDFLAGS="-L/path/to/ncnn/build/src -lc++ -lomp"
fpm test
```

`--link-flag` does the same job per invocation:
`fpm build --link-flag "-L/path/to/ncnn/build/src" --link-flag "-lc++"`.

Note that the sources must be compiled in dependency order (`ncnn_types` before
everything that uses it, and so on). fpm derives the order from the `use`
statements, so it needs no help; a manual `gfortran src/*.f90` will fail because
shell globbing is alphabetical. Without fpm, compile them explicitly:

```sh
mkdir -p build && cd build
for m in ncnn_types ncnn_core ncnn_mat ncnn_blob ncnn_paramdict ncnn_datareader \
         ncnn_modelbin ncnn_layer ncnn_net ncnn_extractor ncnn; do
    gfortran -c ../src/$m.f90
done
gfortran ../test/test_ncnn.f90 *.o -lncnn -lc++ -lomp -lpthread -o test_ncnn
```

## Example

Runs squeezenet_v1.1 on a constant grey image and prints the top class:

```sh
fpm run --example squeezenet
# or, without fpm:
FORNCNN_MODEL_DIR=/path/to/squeezenet_v1.1 ./squeezenet
```

Model files are not vendored. Get `squeezenet_v1.1.param` and
`squeezenet_v1.1.bin` from
[ncnn-assets](https://github.com/nihui/ncnn-assets/tree/master/models) and point
`FORNCNN_MODEL_DIR` at the directory containing them.

The test suite runs the same inference when `FORNCNN_MODEL_DIR` is set, and
skips that section when it is not.

## Naming

Fortran has no `ns::x` scope resolution: `use` flattens a module's public
entities into the caller's scope. A 200-function binding therefore has to
disambiguate by name. The scheme here is:

- One module per C API section: `ncnn_mat`, `ncnn_net`, `ncnn_extractor`, …
- Procedure names drop the leading `ncnn_` and keep the section prefix:
  `ncnn_mat_create_2d` → `mat_create_2d`.
- Handle types keep the C names with the `_t` suffix, because a module and a
  type cannot share a name in Fortran and type names are the most
  collision-prone: `ncnn_mat_t`, `ncnn_net_t`, …
- `module ncnn` is an umbrella that re-exports everything for `use ncnn`.

So the mapping to the C API is mechanical:

```fortran
use ncnn
type(ncnn_mat_t)     :: in
type(ncnn_net_t)     :: net
type(ncnn_extractor_t) :: ex

net = net_create()
call net_load_param(net, 'squeezenet_v1.1.param')
call net_load_model(net, 'squeezenet_v1.1.bin')

in = mat_create_3d(227, 227, 3, alloc)
call mat_fill_float(in, 128.0_c_float)
call mat_substract_mean_normalize(in, mean_vals, norm_vals)

ex = extractor_create(net)
call extractor_input(ex, 'data', in)
call extractor_extract(ex, 'prob', out, stat)
```

Prefer `use, only:` on the sub-modules when you want to control exactly what
enters scope:

```fortran
use ncnn_net,       only: ncnn_net_t, net_create, net_load_param
use ncnn_mat,       only: ncnn_mat_t, mat_create_3d, mat_fill_float
use ncnn_extractor, only: ncnn_extractor_t, extractor_create, extractor_input
```

## Ownership

Every handle is a single `c_ptr`. There are deliberately **no finalisers**:
handles are copyable, so a finaliser would double-free when two copies of a
handle go out of scope. Each `*_create` / `*_destroy` pair must be matched by
hand, and a destroy nulls the handle so a second destroy is safe.

The pool allocator aborts at teardown if a Mat is still outstanding, so free
every Mat before destroying the allocator it came from.

Two further lifetime rules that the C API leaves implicit:

- `mat_create_external_*` wraps your buffer instead of copying it. The buffer
  must be contiguous and must outlive the Mat, and it must not be a
  non-contiguous section — the compiler would silently copy it and ncnn would
  be left with a dangling pointer.
- `mat_reshape_*` returns a Mat that shares storage with its source. Both must
  be destroyed.

## Index conventions

- Mat channel and batch indices are **1-based** (`mat_get_channel_data(m, 1)`).
- Layer and blob indices are **0-based**, matching C (`layer_get_bottom(l, 0)`).
- `blob_get_producer` / `blob_get_consumer` and the indices returned by
  `net_get_input_index` / `net_get_output_index` are 0-based, as in C.
- The `option` `use_*` flags are plain `integer`s, 0 or 1, exactly as the C API
  has them.

## Data access

`mat_get_data`, `mat_get_channel_data` and `mat_get_batch_data` return
`real(c_float)` and require `elemsize == 4`; they stop with an error otherwise
rather than over-reading. Use `mat_get_data_bytes` for raw bytes of any
element type.

Sizes are computed from `cstep * c`, which is what ncnn's `Mat::total()` uses.
Note that `cstep` is padded for alignment, so a 4x3 fp32 Mat reports 12
elements while a 4x3 fp16 Mat reports 32 bytes, not 24.

## Coverage

Bound: **177** of the C API's functions, as 178 Fortran entry points (the extra
one is `mat_get_data_bytes`, which has no C counterpart).

| Module | Entry points | Notes |
|---|---|---|
| `ncnn_mat` | 53 | 52 C functions + `mat_get_data_bytes` |
| `ncnn_core` | 47 | version, allocator, option |
| `ncnn_layer` | 32 | includes the two `ncnn_blob_get_*_shape` functions, which take a layer |
| `ncnn_net` | 20 | |
| `ncnn_paramdict` | 9 | |
| `ncnn_extractor` | 7 | |
| `ncnn_blob` | 4 | |
| `ncnn_datareader` | 3 | |
| `ncnn_modelbin` | 3 | |
| `ncnn_types` | — | handle types, `c_f_string`, `fill_c_string` |
| `ncnn` | — | umbrella |

### Intentionally not bound

**Pixel API** — `ncnn_mat_from_pixels` (4), `ncnn_mat_to_pixels` (2) and
`ncnn_draw_*` (16). These all take `unsigned char*`, which Fortran has no
equivalent for. ncnn does ship `src/simpleocv` (a minimal OpenCV subset with
`imread`/`imwrite`/`resize`/`cvtColor`), but it is off by default
(`NCNN_SIMPLEOCV=OFF`), is pure C++ with no `extern "C"`, and is not exposed
through `c_api` at all.

Dropping it does not block image models: read the image yourself, convert to
`real(c_float)`, wrap it with `mat_create_external_3d` (which is fixed at
`elemsize == 4`), then normalise with `mat_substract_mean_normalize`:

```fortran
! rgb(:) is HWC uint8 read by whatever image library you use
f = real(rgb, kind=c_float)
in = mat_create_external_3d(w, h, 3, f, alloc)
call mat_substract_mean_normalize(in, mean_vals, norm_vals)
```

**`ncnn_datareader_create_from_stdio`** — takes a `FILE*`, which Fortran cannot
portably produce. Use `datareader_create_from_memory` with the file contents
read into a byte array.

**`ncnn_net_register_custom_layer_by_type` / `_by_typeindex`** — these take a
layer creator callback, but the layer's method table (`load_param`,
`load_model`, `create_pipeline`, `forward_*`) lives inside the `ncnn_layer_t`
struct as function pointers rather than as standalone C functions. Binding them
would mean reproducing the struct layout in Fortran and dispatching through
`c_f_procpointer`, so implementing a custom layer body in Fortran is not
supported.

Consequently `layer_get_type` and `layer_get_name` return empty strings for a
layer you create yourself: ncnn only fills `Layer::type` and `Layer::name` in
`Layer::load_param`, which the C API does not expose. Use
`layer_get_typeindex` as the identifier.

**Vulkan** — `ncnn_pipelinecache_*` (10), `ncnn_option_set_pipeline_cache` and
`ncnn_net_set_vulkan_device` are behind `#if NCNN_VULKAN`. Bind them if you
build ncnn with Vulkan.

**Windows** — `ncnn_net_load_param_w` and friends take `wchar_t*` and are
`#if _WIN32`.

## Licence

BSD 3-Clause, the same as ncnn itself. See `LICENSE`.
