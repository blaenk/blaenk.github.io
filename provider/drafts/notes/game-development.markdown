---
title: Game Development
published: September 6, 2013
excerpt: My experiences from creating a game
comments: off
toc: off
---

I've been wanting to create a very specific kind of game for a few years now. Game Development was _one_ of the reasons why I initially became interested in software development, but I quickly lost interest in it relative to other areas that piqued my interests. Still, I was always interested in Game Development, particularly with how it seemed to me like a tour de force in software development, requiring expertise in a variety of areas in order to produce a working game.

Over the years I've tried to learn about various different areas of software development --- as different as possible --- to gain as much perspective and become more well rounded. These new things I've learned have provided me with insight and a "bigger picture" with regards to game development, to the point where I feel more strongly than before that I should at least attempt to create the game I've had in mind. For preparation, last year I read a book on [3D Math](/reads/#3dmath) that solely concerned itself with the math, and one on [Direct3D 11](/reads/#d3d11).

# Build System

The best build system in my opinion is [CMake](/notes/cmake/), which can generate build files on various platforms. Perhaps the only thing --- as minor as it may be --- that bothered me about developing on Windows was the fact that all settings were mainly configured through a GUI, hidden in various sections and so on. This felt very haphazard to me, mainly difficult to grok which settings were manually specified --- they are emboldened in the GUI, but scattered throughout various property sheets, sections, and dialogs.

With CMake, I can define all of the settings I want in a CMake file and have a neat Visual Studio project be generated from it. Likewise on POSIX systems, Makefiles are automatically created.

# Rendering

One of the major questions in engine architecture is the [decoupling of rendering from actors](http://gamedev.stackexchange.com/questions/14133/should-actors-in-a-game-be-responsible-for-drawing-themselves). It seemed to me that having the actors draw themselves contradicted very fundamental software development principles, such as [single responsibility](http://en.wikipedia.org/wiki/Single_responsibility_principle) and [separation of concerns](http://en.wikipedia.org/wiki/Separation_of_concerns).

The main idea behind the solution to this problem, that I have read about, is to define a separate renderer class. This is the class that will concern itself with rendering things.

There is a common data structure called a [scene graph](http://en.wikipedia.org/wiki/Scene_graph) that embeds a hierarchical representation with regards to coordinate space relationships, specifically for the purpose of facilitating transformations on groups of related objects. For example, a player mesh that holds a weapon mesh in its hands would be represented in the scene graph as a player node with a weapon child node. If the player node is translated or rotated, the transformation will carry through to the weapon node --- as would be logically expected.

Aside from this scene graph representation, there would also be another data structure for the purposes of [space partitioning](http://en.wikipedia.org/wiki/Space_partitioning), to facilitate visibility testing, such as [Binary Space Partitions](http://en.wikipedia.org/wiki/Binary_space_partitioning) or [Octrees](http://en.wikipedia.org/wiki/Octree).

If using an octree, keep some [threshold](http://gamedev.stackexchange.com/questions/44038/octree-implementation-for-fustrum-culling/) number of meshes per node to avoid excessive partitioning. Similarly, if a mesh fits in multiple partitions then it should appear in each of them. When objects move, the list of meshes in each partition should be modified to represent this change. Too many or too few meshes in a partition might require partitioning or merging, respectively.

The rendering phase can then be divided into visibility determination and actual rendering. The octree is traversed to collect visible objects. These objects are added to specific buckets pertaining to their shader type, and during rendering these buckets are rendered sequentially to avoid unnecessary/duplicate state changes.

The engine I'm creating so far consists of actors that are component-based. I feel that perhaps it would be possible to define a mesh component, for example, that contains relevant information. The actors can use the [visitor pattern](http://en.wikipedia.org/wiki/Visitor_pattern) so that, on traversal of the scene graph, the renderer calls the actor's render method which fetches any renderable components and delegates the rendering back to the renderer. **TODO**: Is that a good idea? What would constitute a "renderable" component? Enforcing this in static code would seemingly do away with the benefits of run-time composition?

Resources:

* [Should an object in a 2D game render itself?](http://gamedev.stackexchange.com/questions/13492/should-an-object-in-a-2d-game-render-itself)
* [Should actors in a game be responsible for drawing themselves?](http://gamedev.stackexchange.com/questions/14133/should-actors-in-a-game-be-responsible-for-drawing-themselves)
* [Tactics for moving the render logic out of the GameObject class](http://gamedev.stackexchange.com/questions/29471/tactics-for-moving-the-render-logic-out-of-the-gameobject-class)
* [Separation of game and rendering logic](http://stackoverflow.com/questions/2756655/separation-of-game-and-rendering-logic)
* [Scene graphs and spatial partitioning structures: What do you really need?](http://gamedev.stackexchange.com/questions/41872/scene-graphs-and-spatial-partitioning-structures-what-do-you-really-need)

# Voxel Engine

I'd like to have voxel-based terrain --- no, it's not a minecraft clone --- so it's important to keep in mind that there are a variety of efficiency questions involved in creating a robust engine for this kind of world.

## Chunks

The most common optimization is to treat the world as a composition of **chunks**, each chunk being a composition of WxHxD voxels, Minecraft [uses a](http://www.minecraftwiki.net/wiki/Chunks) logical chunk size of 16x16x256 and graphics chunk size of 16x16x16. Chunks have a variety of purposes. One of them is that they do away with having to instantiate a multitude of individual voxel objects. Instead, chunks are in charge of keeping the information of the blocks that constitute it.

A chunk may be represented by a volumetric grid such as a 3D array, optimized into a flat slab of memory indexed with the formula:

$$ chunk[x + (y * size_x) + (z * size_x * size_y)] $$

The problem with this is that a lot of space is wasted for empty cells. A 16x16x16 chunk size where every cell conveys its information within one byte of data --- for example, for its block type: lava, grass, etc. --- yields 4096 bytes, or 4 kilobytes. If every single cell in the chunk is empty except for one, then 4095 bytes are being wasted --- that's 99.9% of the space allocated for the chunk.

One solution to this is to use something similar to a [sparse matrix](http://en.wikipedia.org/wiki/Sparse_matrix). Specifically, the chunk can be represented by a hash table keyed by tuples corresponding to the coordinates within the chunk. This way all cells are implicitly empty cells unless present in the hash table. During the mesh construction phase, the hash table can be iterated for every non-empty cell. Similarly, the hash table can provide amortized constant access to specific cells in the chunk.

Another common solution to this is to use [Run-Length Encoding](http://en.wikipedia.org/wiki/Run-length_encoding) compression on the volumetric grid. However, this probably doesn't have the same near-constant access benefit as hash tables.

## Overdraw

One problem with voxel terrain is that it easily allows for overdraw, where non-visible geometry is passed to the GPU for rendering. Non-visible geometry in this case refers to the traditional cases of geometry being outside of the view frustum and geometry occluded by other geometry.

In voxel engines, however, the second case --- occluded geometry --- can also be very important with regards to inter-chunk, non-visible faces. That is, given a 2x2x2 chunk, there is no need to render the faces that intersect the chunk --- the inner faces. The natural solution to this is to [cull the interior faces](http://0fps.wordpress.com/2012/06/30/meshing-in-a-minecraft-game/) by querying the neighbors during chunk geometry generation.

A further optimization is back-face culling. This can be done on the GPU based on the ordering of the vertices, but it can also be done on the CPU thereby avoiding the unnecessary transfer of the data. A way to accomplish this is to have separate vertex buffers for the different sides of the chunk. Then, depending on the position of the camera, only sending those buffers that are visible.

Another optimization yet is a higher-level one that would occur before most other ones is known as frustum culling: disregarding geometry that isn't within the camera's [view frustum](http://en.wikipedia.org/wiki/Viewing_frustum). This would be performed at chunk-level granularity. An octree can be used to quickly determine chunks that are visible by the camera.

* [OpenGL Volume Rendering](http://bytebash.com/2012/03/opengl-volume-rendering/): very nice modern C++11
* [GLBlox](https://github.com/pabennett/glblox): from the above. nice modern C++11
* [Something other than Vertex Welding with Texture Atlas?](http://gamedev.stackexchange.com/questions/57716/something-other-than-vertex-welding-with-texture-atlas/)
* [Voxel Face Crawling (Mesh simplification, possibly using greedy)](http://gamedev.stackexchange.com/questions/58527/voxel-face-crawling-mesh-simplification-possibly-using-greedy)
* [An Analysis of Minecraft-like Engines](http://0fps.wordpress.com/2012/01/14/an-analysis-of-minecraft-like-engines/)
* [Meshing in a Minecraft Game](http://0fps.wordpress.com/2012/06/30/meshing-in-a-minecraft-game/)
* [Ambient Occlusion for Minecraft-like worlds](http://0fps.wordpress.com/2013/07/03/ambient-occlusion-for-minecraft-like-worlds/)
* [Texture atlases, wrapping and mip mapping](http://0fps.wordpress.com/2013/07/09/texture-atlases-wrapping-and-mip-mapping/)
* [Sea of Memes](http://www.sea-of-memes.com/summary/blog_parts.html)
* [Polygon Triangulation](http://en.wikipedia.org/wiki/Polygon_triangulation)
* [Simple, Fast Polygon Reduction Algorithm](http://dev.gameres.com/program/visual/3d/PolygonReduction.pdf)
* [Smooth Voxel Terrain](http://0fps.wordpress.com/2012/07/10/smooth-voxel-terrain-part-1/)
* [Polygonising a Scalar field](http://paulbourke.net/geometry/polygonise/)
* [Triangulation](http://paulbourke.net/papers/triangulate/)
* [Minecraft from a Developer's Perspective](http://www.youtube.com/watch?v=dTFkmfnkCfk)
* [In what kind of variable type is the player position stored on a MMORPG such as WoW?](http://gamedev.stackexchange.com/questions/3935/in-what-kind-of-variable-type-is-the-player-position-stored-on-a-mmorpg-such-as)
* [Dungeon Seige Publications](http://scottbilas.com/games/dungeon-siege/)
* [For voxel rendering, what is more efficient: pre-made VBO or a geometry shader?](http://gamedev.stackexchange.com/questions/17171/for-voxel-rendering-what-is-more-efficient-pre-made-vbo-or-a-geometry-shader/17225#17225)
* [Computing normals in fragment shaders](https://bitbucket.org/volumesoffun/polyvox/wiki/Computing%20normals%20in%20a%20fragment%20shader)
* [PolyVox](http://www.volumesoffun.com/polyvox-about/)
* [More on Minecraft-type world gen](http://www.gamedev.net/blog/33/entry-2227887-more-on-minecraft-type-world-gen/)
* [More Procedural Voxel World Generation](http://www.gamedev.net/blog/33/entry-2249106-more-procedural-voxel-world-generation/)
* [Revisiting the block worlds stuff](http://www.gamedev.net/blog/33/entry-2258571-revisiting-the-block-worlds-stuff/)
* [Terasology](http://blog.movingblocks.net/blockmania/), [Source](https://github.com/MovingBlocks/Terasology)
* [From Voxels to Polygons](http://procworld.blogspot.com/2010/11/from-voxels-to-polygons.html)
* [Voxel Relaxation](http://procworld.blogspot.com/2010/11/just-relax.html)

# Resources

* [Design Patterns as they relate to Game Development](http://gameprogrammingpatterns.com/index.html)
