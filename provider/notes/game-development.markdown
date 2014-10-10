---
title: Game Development
published: September 6, 2013
excerpt: My experiences from creating a game
comments: off
toc: right
---

* toc

I've been wanting to create a very specific kind of game for a few years now. Game Development was _one_ of the reasons why I initially became interested in software development, but I quickly lost interest in it relative to other areas that piqued my interests. Still, I was always interested in Game Development, particularly with how it seemed to me like a tour de force in software development, requiring expertise in a variety of areas in order to produce a working game.

Over the years I've tried to learn about various different areas of software development, as orthogonal as possible, to gain as much perspective and become more well rounded. These new things I've learned have provided me with insight and a "bigger picture" with regards to game development, to the point where I feel more strongly than before that I should at least attempt to create the game I've had in mind. For preparation, last year I read a book on [3D Math](/reads/#3dmath) that solely concerned itself with the math, and one on [Direct3D 11](/reads/#d3d11).

# Architecture

## New Approach

I'm taking a step back from the overly general architecture discussed in the following sections, consisting of "components everywhere" and a very loose, omnipresent event system that sacrificed the type system. I originally chose to create my engine from scratch because I wanted to use this as a learning experience, but also to have absolute control over what I want to create. However, I'm well aware of the countless games that roll their own engine only to still be working on the engine for years with no game to show for it. I don't want that to happen here.

My opinion is that this tends to happen because people want to make their engine be everything: fully cross-platform and fully general so that it can scale from a mobile F2P game to a next-gen racing game and further still to an FPS. I'm reminded of the days of the Quake 3 engine, which---despite [my limited experience with it](/work/#the-instagib-project)---didn't seem to strive to be everything for everyone, and yet many people adapted it to accommodate all manner of different game types.

There is clearly a balance between creating a game and creating a general engine. The end goal is the game, but it's not necessary to completely sacrifice good software architecture practices, specifically those associated with game engine design (e.g. components, event systems, etc.).

The approach I'm going to try now is to focus on creating a game, leaning towards a game-specific engine, while being sensitive to potential refactorings. These potential refactorings will be a lot clearer to me than to write them from the beginning, since I will have a concrete example of exactly where and why they might be useful.

Anyone who has ever tried to learn anything about game engine design is familiar with the ever-present author disclaimer that goes something like "keep in mind this works in our particular engine, but circumstances in your engine may differ." Being new to game engine design, the task of designing a well architected, flexible engine from the start seems too open-ended. That's why I'm taking this approach, in order to help guide the process.

## Components

Every `Actor` has a collection of `Component` objects which compose to define the whole of a particular `Actor`, including its attributes and behavior. Some `Components` are tied to a specific `Engine` subsystem, such as `RenderableComponent`. All these do is notify the particular subsystem of their existence so that they can be used. For example, the `Renderer` subsystem would receive the event from a `MeshComponent` and so would update its list of meshes to render on the next frame.

* [entity framework](http://www.richardlord.net/blog/what-is-an-entity-framework)
* [ash entity engine](http://shaun.boyblack.co.za/blog/2012/08/04/games-and-entity-systems/)
* [component based engine design](http://www.randygaul.net/2013/05/20/component-based-engine-design/)
* [game objects](http://www.gamearchitect.net/Articles/GameObjects1.html)

## Events

The `Engine` has Components/Susbystems such as `Renderer`, `Game`, `Audio`, etc. `Game` has collection of `Actors`, and `Actors` have collection of `Components`. This hierarchy forms a chain of responsibility up to the `Engine`.

<img class="center" src="/images/notes/game-development/engine-architecture.png">

The chain of responsibility forms an event-handler hierarchy. Each `Event` is derived from a base event that has `scope` and `subject` properties.

The `scope` consists of the system the event is meant for. For example, a `Component` event would be `Component`, whereas an event meant for the renderer would be `Renderer`. This allows fast forwarding of events through the chain of responsibility.

The `subject` itself is the event's identifier, for example, "player died." A particular derived `Event` would have appropriate data for this event which might be necessary for its handling.

To forgo Run-Time Type Information (RTTI) and its associated performance cost, some sort of lookup table is used to determine the type of message to then `static_cast` it to the appropriate event, in order to allow simply having an `OnEvent`-type function with a base `Event` parameter.

For example (tentative), when a `MeshComponent` is added to an `Actor`, it'll want to register itself with the `Renderer` by sending a `MeshAdded` event which contains a `shared_ptr` to the `MeshComponent`. The `MeshAdded` event might look something like this:

``` cpp
class MeshAdded : public RendererEvent {
public:
  MeshAdded(std::shared_ptr<MeshComponent> mesh) :
    subject_(RendererEvent::Subject::MeshAdded),
    mesh_(mesh) {}
  std::shared_ptr<MeshComponent> mesh() const;
private:
  std::shared_ptr<MeshComponent> mesh_;
};
```

When a `MeshComponent` is added to an `Actor`, it sends this `MeshAdded` event to the `Actor` because it is the nearest possible handler in the chain of responsibility. The `Actor` recognizes that it is out of its scope, so it passes it up the chain of responsibility to the `Game`, which does the same. When the event reaches the `Engine`, it forwards the event to the `Renderer` component which finally handles it appropriately. The `Renderer` handles it by adding the mesh to its list of meshes to be rendered on the next frame. When the `MeshComponent` is removed from the `Actor`, it sends an event so that the mesh is removed from the list.

The per-event `scope` allows the chain of responsibility to fast-forward events which are out of the current handler system's scope. For example, in the example above, as soon as the `Actor` and `Game` systems noticed the event of scope `Renderer`, it didn't bother to see if any of its registered handlers could handle the event and instead forwarded it up the chain as soon as possible.

Similarly, it could be beneficial for each system to allow components to register interest in events. With such a system, if there are 100 `Actors` but only 2 of them are interested in a particular event, only those 2 interested `Actors` would be sent the event instead sending the event to all 100 `Actors` to determine which are capable of handling it. This can be accomplished by a map from event subject to a list of interested handlers:

``` cpp
std::unordered_map<GameEvent::Subject,
                   std::vector<std::shared_ptr<Actor>>> handlers_;

handlers[GameEvent::Subject::PlayerDied].push_back(someActor);
```

The event loop for `Game` can then look something like this:

``` cpp
void OnEvent(const Event &event) {
  if (event.scope() != Event::Scope::Game) {
    this->parent_->OnEvent(event);
    return;
  }

  for (auto &handler : handlers[event.subject()])
    handler.OnEvent(event);
}
```

## Input

Have an `Input` subsystem. In the SDL event loop, feed key events to the `Input` subsystem. The `Game` can be a mediator. The `InputComponents` register interest in `Game` actions such as `Jump`. The `Game` contains a map of the key bindings mapping action-to-key, as well as a map of action-to-components. If an action has even one registered `InputComponent`, then the `Game` registers an interest in the key for that action with the `Input` subsystem. When the `Input` subsystem detects that this key is pressed, for example, it notifies the `Game` of this, which translates it into a `Game` action and forwards it to every registered `InputComponent`.

* [robust input handling](http://www.gamedev.net/blog/355/entry-2250186-designing-a-robust-input-handling-system-for-games/)
* [input in component-based systems](http://gamedev.stackexchange.com/questions/48315/game-state-and-input-handling-in-component-based-entity-systems)
* [input in component-based system #2](http://gamedev.stackexchange.com/questions/49119/input-handling-in-component-based-design)
* [input in large games](http://gamedev.stackexchange.com/questions/59582/input-management-techniques-in-large-games)

## Optimizations

_Object state caching_ entails an object keeping a "double buffer" of its state: one for the previously calculated state, and another for the to-be calculated state. This helps, but doesn't completely solve, the problem of one-frame-off lag. It also allows for easy interpolation between two states for easy feeding into an interpolating render function. This only guarantees that the previous frame's state is fully consistent, but not the current state.

* [quaternion rotation without euler angles](http://stackoverflow.com/questions/17044296/quaternion-rotation-without-euler-angles/17047931#17047931)
* [renderable components](http://gamedev.stackexchange.com/questions/47079/component-based-object-traversal)
* [physics and renderable components](http://gamedev.stackexchange.com/questions/13797/how-are-physics-or-graphics-components-typically-built-in-a-component-oriented-s)
* [order draw calls](http://realtimecollisiondetection.net/blog/?p=86)
* [materials in a component system](http://gamedev.stackexchange.com/questions/47117/how-to-handle-materials-in-an-entity-component-system)
* [Evolve your hierarchy](http://cowboyprogramming.com/2007/01/05/evolve-your-heirachy/)
* [Custom 2D Physics engine core](http://gamedev.tutsplus.com/tutorials/implementation/how-to-create-a-custom-2d-physics-engine-the-core-engine/)
* [Component based games in practice](http://gamedev.stackexchange.com/questions/4637/component-based-game-object-systems-in-practice)
* [Component based object traversal](http://gamedev.stackexchange.com/questions/47079/component-based-object-traversal)
* [Should an object in a 2D game render itself?](http://gamedev.stackexchange.com/questions/13492/should-an-object-in-a-2d-game-render-itself)
* [Should actors in a game be responsible for drawing themselves?](http://gamedev.stackexchange.com/questions/14133/should-actors-in-a-game-be-responsible-for-drawing-themselves)
* [Tactics for moving the render logic out of the GameObject class](http://gamedev.stackexchange.com/questions/29471/tactics-for-moving-the-render-logic-out-of-the-gameobject-class)
* [Separation of game and rendering logic](http://stackoverflow.com/questions/2756655/separation-of-game-and-rendering-logic)
* [Scene graphs and spatial partitioning structures: What do you really need?](http://gamedev.stackexchange.com/questions/41872/scene-graphs-and-spatial-partitioning-structures-what-do-you-really-need)

# Build System

The best build system for a game in my opinion is [CMake](/notes/cmake/), which can generate build files on various platforms. Perhaps the only thing that bothered me about developing on Windows was the fact that all settings were mainly configured through a GUI, hidden in various sections and so on. This felt very haphazard to me, mainly difficult to grok which settings were manually specified.

With CMake, I can define all of the settings I want in a CMake file and have a neat Visual Studio project be generated from it. Likewise on POSIX systems, Makefiles are automatically created.

# Game Loop

The game loop isn't as cut and dry as I used to think. There are a variety of considerations:

* [Game Loop](http://gameprogrammingpatterns.com/game-loop.html)
* [Fix your timestep](http://gafferongames.com/game-physics/fix-your-timestep/)
* [deWitter's game loop](http://www.koonsolo.com/news/dewitters-gameloop/)
* [Fixed time step vs variable time step](http://gamedev.stackexchange.com/questions/1589/fixed-time-step-vs-variable-time-step)
* [Casey and the clearly deterministic contraptions](http://gamesfromwithin.com/casey-and-the-clearly-deterministic-contraptions)

# Rendering

One of the major questions in engine architecture is the [decoupling of rendering from actors](http://gamedev.stackexchange.com/questions/14133/should-actors-in-a-game-be-responsible-for-drawing-themselves). It seemed to me that having the actors draw themselves contradicted very fundamental software development principles, such as [single responsibility](http://en.wikipedia.org/wiki/Single_responsibility_principle) and [separation of concerns](http://en.wikipedia.org/wiki/Separation_of_concerns).

The main idea behind the solution to this problem, that I have read about, is to define a separate renderer class. This is the class that will concern itself with rendering things.

There is a common data structure called a [scene graph](http://en.wikipedia.org/wiki/Scene_graph) that is a hierarchical representation of coordinate space relationships, specifically for the purpose of facilitating transformations on groups of related objects. For example, a player mesh that holds a weapon mesh in its hands would be represented in the scene graph as a player node with a weapon child node. If the player node is translated or rotated, the transformation will carry through to the weapon node.

Aside from this scene graph representation, there would also be another data structure for the purposes of [space partitioning](http://en.wikipedia.org/wiki/Space_partitioning), to facilitate visibility testing, such as [Binary Space Partitions](http://en.wikipedia.org/wiki/Binary_space_partitioning) or [Octrees](http://en.wikipedia.org/wiki/Octree).

If using an octree, keep some [threshold](http://gamedev.stackexchange.com/questions/44038/octree-implementation-for-fustrum-culling/) number of meshes per node to avoid excessive partitioning. Similarly, if a mesh fits in multiple partitions then it should appear in each of them. When objects move, the list of meshes in each partition should be modified to represent this change. Too many or too few meshes in a partition might require partitioning or merging, respectively.

The rendering phase can then be divided into visibility determination and actual rendering. The octree is traversed to collect visible objects. These objects are added to specific buckets pertaining to their shader type, and during rendering these buckets are rendered sequentially to avoid unnecessary/duplicate state changes.

The engine I'm creating so far consists of actors that are component-based. I feel that perhaps it would be possible to define a mesh component, for example, that contains relevant information. The actors can use the [visitor pattern](http://en.wikipedia.org/wiki/Visitor_pattern) so that, on traversal of the scene graph, the renderer calls the actor's render method which fetches any renderable components and delegates the rendering back to the renderer. **TODO**: Is that a good idea? What would constitute a "renderable" component? Enforcing this in static code would seemingly do away with the benefits of run-time composition?

## Major Notation

I learned linear algebra and related 3D math using math and Direct3D texts which both used left-handed coordinate systems and row-major notation. Transitioning to the right-handed system of OpenGL, I was aware of some of the differences, such as reversing the order of matrix concatenations, but I was a bit uncertain about everything entailed in usng the different system. I found a [pretty good article](http://seanmiddleditch.com/journal/2012/08/matrices-handedness-pre-and-post-multiplication-row-vs-column-major-and-notations/) on the topic. What follows are my notes from reading that article.

### Handedness

The handedness of a system has an effect on the directions of the positive $z$-axis, positive rotation, and the cross-product. These can easily be determined by using the [right-hand rule](http://en.wikipedia.org/wiki/Right-hand_rule) or left-hand rule. It's important to realize that this has _nothing_ to do with row or column-major notation.

### Representation

Row-major notation means that when indexing or denoting a matrix' size, the row is written first, and vice versa with column-major notation. Given a row-major matrix $R$, the column-major matrix $C$ is derived by deriving the transpose $R^{T}$:

$$
\begin{align}
R &=
\begin{bmatrix}
  1 & 2 & 3 \\
  4 & 5 & 6
\end{bmatrix}
\\
\\
C = R^T &=
\begin{bmatrix}
  1 & 4 \\
  2 & 5 \\
  3 & 6
\end{bmatrix}
\end{align}
$$


### Storage

When it comes to matrix storage in memory, the key is that they are both stored the same way. A row-major matrix is stored in contiguous memory one row at a time, whereas a column-major matrix is stored one column at a time. The _implication_ is that a stored row-major matrix can be passed to an API that expects column-major matrices _without_ needing to transpose the matrix first.

### Multiplication

When multiplying vectors with matrices, row-major notation requires pre-multiplication; the vector must appear to the left of the matrix. Column-major notation requires post-multiplication. This has the effect that the row-major, pre-multiplication transformation:

$$ vMVP $$

Must be expressed as follows in column-major notation:

$$ M^T V^T P^T v^T $$

However, row-major and column-major matrices are already transposes of each other, and as a result they are [stored](#storage) the same way in memory. This means that no actual transposing has to take place!

# Voxel Engine

I'd like to have voxel-based terrain (no, it's not a minecraft clone) so it's important to keep in mind that there are a variety of efficiency questions involved in creating a robust engine for this kind of world.

## Chunks

The most common optimization is to treat the world as a composition of _chunks_, each chunk being a composition of WxHxD voxels, Minecraft [uses a](http://www.minecraftwiki.net/wiki/Chunks) logical chunk size of 16x16x256 and graphics chunk size of 16x16x16. Chunks have a variety of purposes. One of them is that they do away with having to instantiate a multitude of individual voxel objects. Instead, chunks are in charge of keeping the information of the blocks that constitute it.

A chunk may be represented by a volumetric grid such as a 3D array, optimized into a flat slab of memory indexed with the formula:

$$ \text {chunk}[x + (y * \text {size}_x) + (z * \text {size}_x * \text {size}_y)] $$

The problem with this is that a lot of space is wasted for empty cells. A 16x16x16 chunk size where every cell conveys its information within one byte of data yields 4096 bytes, or 4 kilobytes. If every single cell in the chunk is empty except for one, then 4095 bytes are being wasted. That's 99.9% of the space allocated for the chunk.

One solution to this is to use something similar to a [sparse matrix](http://en.wikipedia.org/wiki/Sparse_matrix). Specifically, the chunk can be represented by a hash table keyed by tuples corresponding to the coordinates within the chunk. This way all cells are implicitly empty cells unless present in the hash table. During the mesh construction phase, the hash table can be iterated for every non-empty cell. Similarly, the hash table can provide amortized constant access to specific cells in the chunk.

Another common solution to this is to use [Run-Length Encoding](http://en.wikipedia.org/wiki/Run-length_encoding) compression on the volumetric grid. However, this probably doesn't have the same near-constant access benefit as hash tables.

## Overdraw

One problem with voxel terrain is that it easily allows for overdraw, where non-visible geometry is passed to the GPU for rendering. Non-visible geometry in this case refers to the traditional cases of geometry being outside of the view frustum and geometry occluded by other geometry.

In voxel engines, however, the second case---occluded geometry---can also be very important with regards to inter-chunk, non-visible faces. That is, given a 2x2x2 chunk, there is no need to render the faces that intersect the chunk, i.e. the inner faces. The natural solution to this is to [cull the interior faces](http://0fps.wordpress.com/2012/06/30/meshing-in-a-minecraft-game/) by querying the neighbors during chunk geometry generation.

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
* [Terasology](http://blog.movingblocks.net/blockmania/), [Source](https://github.com/MovingBlocks/Terasology)
* [From Voxels to Polygons](http://procworld.blogspot.com/2010/11/from-voxels-to-polygons.html)
* [Voxel Relaxation](http://procworld.blogspot.com/2010/11/just-relax.html)

## Terrain Generation

Infinite worlds have eventual limits mainly due to round-off errors with floating point numbers. Minecraft tries to mitigate this by using local coordinates:

> Many of these problems can be solved by changing the math into a local model centered around the player so the numbers all have vaguely the same magnitude. For rendering, Minecraft already uses local coordinates within the block and offset the block position relative to the player to give the impression of the player moving. This is mostly due to OpenGL using 32 bit floats for positions, but also because the rounding errors are extremely visible when displayed on a screen.
>
> <cite><strong>Notch</strong> on <a href="http://notch.tumblr.com/post/3746989361/terrain-generation-part-1">Terrain Generation</a></cite>

Terrain generation is usually accomplished with noise:

* [Terasology Terrain Generation](https://github.com/MovingBlocks/Terasology/wiki/World-Terrain-Generation)
* [Simplex Noise](http://webstaff.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf)
* [Perlin Noise](http://freespace.virgin.net/hugo.elias/models/m_perlin.htm): See section on wood textures
* [Perlin Noise 2](http://devmag.org.za/2009/04/25/perlin-noise/)
* [Terrain Generation: Hills](http://forums.bukkit.org/threads/the-always-up-to-date-definitive-guide-to-terrain-generation-part-four-hills.94164/)
* [Terrain Generation: 3D Perlin Noise](https://forums.bukkit.org/threads/the-always-up-to-date-definitive-guide-to-terrain-generation-part-5-3d-noise.94312/)
* [Endless Chunks](http://gamedev.stackexchange.com/questions/28970/c-perlin-noise-generating-endless-terrain-chunks)
* [Handling chunk edges](http://www.gamedev.net/topic/639342-infinite-terrain-generation-in-chunks-need-hints-on-handling-chunk-edges/)
* [Tileable Perlin Noise](http://gamedev.stackexchange.com/a/23639/9958)
* [Forum Post](http://www.gamedev.net/topic/631291-marching-cube-holes/?view=findpost&p=4980446)
* [Scale GLM Perlin Noise](https://github.com/dghost/Noisy-Mesh/blob/master/renderwidget.cpp#L205)
* [Procedural Color Palette Generation](http://devmag.org.za/2012/07/29/how-to-choose-colours-procedurally-algorithms/)
* [Perlin Noise Implementation](http://stackoverflow.com/questions/4753055/perlin-noise-generation-for-terrain)
* [More Procedural Voxel World Generation](http://www.gamedev.net/blog/33/entry-2249106-more-procedural-voxel-world-generation/)
* [Revisiting the block worlds stuff](http://www.gamedev.net/blog/33/entry-2258571-revisiting-the-block-worlds-stuff/)
* [More on Minecraft-type world gen](http://www.gamedev.net/blog/33/entry-2227887-more-on-minecraft-type-world-gen/)

# SDL

On Windows, the Visual Studio builds are built using a different run-time library and necessitates building manually with the `/MD` or `/MDd` linker options for Debug and Release respectively to avoid annoying warnings.

The [`SDL_SetWindowFullscreen`](http://wiki.libsdl.org/SDL_SetWindowFullscreen) function takes two parameters as is shown in the wiki page:

* `SDL_WINDOW_FULLSCREEN`: simply sets the window, with the same display mode (res & rr), to be full screen
* `SDL_WINDOW_FULLSCREEN_DESKTOP`: takes the desktop's display mode and fullscreens; very fast alt-tabbing since it doesn't require video mode change?

It seems that, to set a specific display mode, it should first be found by enumerating the display modes with [`SDL_GetNumDisplayModes`](http://wiki.libsdl.org/SDL_GetNumDisplayModes), a specific one retrieved with [`SDL_GetDisplayMode`](http://wiki.libsdl.org/SDL_GetDisplayMode), then set with [`SDL_SetWindowDisplayMode`](http://wiki.libsdl.org/SDL_SetWindowDisplayMode), followed by a call to `SDL_SetWindowFullscreen` with the `SDL_WINDOW_FULLSCREEN` option to change the video mode to fullscreen.

# Resources

* [Design Patterns as they relate to Game Development](http://gameprogrammingpatterns.com/index.html)
* [SDL & OpenGL](http://immersedcode.org/tags/opengl/)
