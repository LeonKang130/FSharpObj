#nowarn "9"
namespace FSharpObj

open System
open System.IO
open System.Numerics
open System.Collections.Generic
open Microsoft.FSharp.NativeInterop

[<Struct>]
type Vertex =
    { position: Vector3
      normal: Vector3
      texcoord: Vector2 }

type Mesh =
    { vertices: Vertex array
      triangles: int array }

module Parsing =
    type private Index =
        { positionIdx: int
          normalIdx: int
          texcoordIdx: int }

        static member Default =
            { positionIdx = 0
              normalIdx = 0
              texcoordIdx = 0 }

    type private MeshAttribute =
        | Position of Vector3
        | Normal of Vector3
        | TexCoord of Vector2
        | Face of Index array

    let private ParseMeshAttribute (line: string) =
        let items = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)

        match items[0] with
        | "v" -> Position(Vector3(Single.Parse items[1], Single.Parse items[2], Single.Parse items[3]))
        | "vn" -> Normal(Vector3(Single.Parse items[1], Single.Parse items[2], Single.Parse items[3]))
        | "vt" -> TexCoord(Vector2(Single.Parse items[1], Single.Parse items[2]))
        | "f" ->
            items
            |> Array.skip 1
            |> Array.map (fun str ->
                let components = str.Split('/', StringSplitOptions.TrimEntries)
                let positionIdx = Int32.Parse components[0]
                let mutable normalIdx = 0
                let mutable texcoordIdx = 0

                if components.Length > 1 then
                    Int32.TryParse(components[1], &texcoordIdx) |> ignore

                if components.Length > 2 then
                    Int32.TryParse(components[2], &normalIdx) |> ignore

                { positionIdx = positionIdx
                  normalIdx = normalIdx
                  texcoordIdx = texcoordIdx })
            |> Face
        | _ -> failwith "Unknown geometry attribute!"

    let ParseOBJ (filename: string) =
        let _, _, _, vertices, triangles, _ =
            use reader = new StreamReader(filename)

            seq {
                while not reader.EndOfStream do
                    let mutable line = reader.ReadLine()

                    if line.Length <> 0 && ((line[0] = 'v' && "tn ".Contains line[1]) || line[0] = 'f') then
                        yield ParseMeshAttribute line
            }
            |> Seq.fold
                (fun
                    (positions: ResizeArray<Vector3>,
                     normals: ResizeArray<Vector3>,
                     texcoords: ResizeArray<Vector2>,
                     vertices: ResizeArray<Vertex>,
                     triangles: ResizeArray<int>,
                     vertexDictionary: Dictionary<Index, int>)
                    attribute ->
                    match attribute with
                    | Position position -> positions.Add position
                    | Normal normal -> normals.Add normal
                    | TexCoord texcoord -> texcoords.Add texcoord
                    | Face indices ->
                        let fixIndex index =
                            let index =
                                { positionIdx =
                                    if index.positionIdx <= 0 then
                                        index.positionIdx + positions.Count
                                    else
                                        index.positionIdx - 1
                                  normalIdx =
                                    if index.normalIdx <= 0 then
                                        index.normalIdx + normals.Count
                                    else
                                        index.normalIdx - 1
                                  texcoordIdx =
                                    if index.texcoordIdx <= 0 then
                                        index.texcoordIdx + texcoords.Count
                                    else
                                        index.texcoordIdx - 1 }

                            if vertexDictionary.TryAdd(index, vertices.Count) then
                                vertices.Add
                                    { position = positions[index.positionIdx]
                                      normal =
                                        if index.normalIdx < normals.Count then
                                            normals[index.normalIdx]
                                        else
                                            Vector3.UnitY
                                      texcoord =
                                        if index.texcoordIdx < texcoords.Count then
                                            texcoords[index.texcoordIdx]
                                        else
                                            Vector2.Zero }

                                vertexDictionary.Count - 1
                            else
                                vertexDictionary.Item index

                        let i = fixIndex indices[0]
                        let mutable j = fixIndex indices[1]

                        for n in 2 .. indices.Length - 1 do
                            let k = fixIndex indices[n]
                            triangles.Add i
                            triangles.Add j
                            triangles.Add k
                            j <- k

                    positions, normals, texcoords, vertices, triangles, vertexDictionary)
                (ResizeArray(), ResizeArray(), ResizeArray(), ResizeArray(), ResizeArray(), Dictionary())

        { vertices = [| for vertex in vertices -> vertex |]
          triangles = [| for index in triangles -> index |] }

module Processing =
    let RecalculateVertexNormal mesh =
        let normals =
            Span(NativePtr.toVoidPtr (NativePtr.stackalloc<Vector3> mesh.vertices.Length), mesh.vertices.Length)

        for n in 0..3 .. mesh.triangles.Length / 3 - 1 do
            let i, j, k = mesh.triangles[n], mesh.triangles[n + 1], mesh.triangles[n + 2]

            let p1, p2, p3 =
                mesh.vertices[i].position, mesh.vertices[j].position, mesh.vertices[k].position

            let normal = Vector3.Normalize(Vector3.Cross(p2 - p1, p3 - p1))
            let w1 = acos (Vector3.Dot(Vector3.Normalize(p2 - p1), Vector3.Normalize(p3 - p1)))
            let w2 = acos (Vector3.Dot(Vector3.Normalize(p3 - p2), Vector3.Normalize(p1 - p2)))
            normals[i] <- normals[i] + normal * w1
            normals[j] <- normals[j] + normal * w2
            normals[k] <- normals[k] + normal * (MathF.PI - w1 - w2)

        let mutable vertices = Array.zeroCreate mesh.vertices.Length

        for i in 0 .. vertices.Length - 1 do
            vertices[i] <-
                { mesh.vertices[i] with
                    normal = Vector3.Normalize normals[i] }

        { mesh with vertices = vertices }
