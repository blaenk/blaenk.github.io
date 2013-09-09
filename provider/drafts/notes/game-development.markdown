---
title: Game Development
published: September 6, 2013
excerpt: My experiences from creating a game
comments: off
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

The engine I'm creating so far consists of actors that are component-based. I feel that perhaps it would be possible to define a mesh component, for example, that contains relevant information. The actors can use the [visitor pattern](http://en.wikipedia.org/wiki/Visitor_pattern) so that, on traversal of the scene graph, the renderer calls the actor's render method which fetches any renderable components and delegates the rendering back to the renderer. **TODO**: Is that a good idea? What would constitute a "renderable" component? Enforcing this in static code would seemingly do away with the benefits of run-time composition?

Resources:

* [Should an object in a 2D game render itself?](http://gamedev.stackexchange.com/questions/13492/should-an-object-in-a-2d-game-render-itself)
* [Should actors in a game be responsible for drawing themselves?](http://gamedev.stackexchange.com/questions/14133/should-actors-in-a-game-be-responsible-for-drawing-themselves)
* [Tactics for moving the render logic out of the GameObject class](http://gamedev.stackexchange.com/questions/29471/tactics-for-moving-the-render-logic-out-of-the-gameobject-class)
* [Separation of game and rendering logic](http://stackoverflow.com/questions/2756655/separation-of-game-and-rendering-logic)
* [Scene graphs and spatial partitioning structures: What do you really need?](http://gamedev.stackexchange.com/questions/41872/scene-graphs-and-spatial-partitioning-structures-what-do-you-really-need)

# Voxel Engine

I'd like to have voxel-based terrain, so it is important to keep in mind that there are a variety of efficiency questions involved in creating a robust engine for this kind of world:

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

