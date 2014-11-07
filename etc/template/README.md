# The Pixbi runtime

This is the runtime used to bootstrap and load content and tags in different
environments.  It's basically a shim on the platform it targets.


## Installation & Usage

See the [Pixbi Frontend Builder](https://github.com/pixbi/build)


## Layers, Tags, and Occlusion

Layers, tags, and occluders possess the following relationships:

* A layer has occluders and tags.
* A layer has a zIndex (integer value).
* A layer is below another layer if the latter has a higher zIndex.

* Occluders and tags are represented as axis-aligned bounding-boxes (AABB).
* Tags are considered occluded if their AAAB intersects with the AABBs of
  occluders in layers above the tag's layer.
* Occluders and tags are positioned relative to a layer.
* Occluders and tags are assumed to have immutable positions and dimensions.
* All tags are assumed to have the same dimensions.

Some considerations:

* As layers may occlude layers below completely, would it make more sense to
  have "windows" rather than occluders?
* All positions/dimensions should be represented as percentages relative to
  the image layer.
* Would work better with thumbnails/different resolutions.
