#nowarn "9"
namespace FSharpObj
open System
open System.IO
open System.Numerics
open System.Collections.Generic
open Microsoft.FSharp.NativeInterop
[<Struct>] type Vertex = { position: Vector3; normal: Vector3; texcoord: Vector2 }
type Mesh = { vertices: Vertex array; triangles: int array }
module Parser =
    [<Struct>] type Index = { positionIdx: int; normalIdx: int; texcoordIdx: int } static member Default = { positionIdx = 0; normalIdx = 0; texcoordIdx = 0 }
    type MeshAttribute =
        | Position of Vector3
        | Normal of Vector3
        | TexCoord of Vector2
        | Face of Index list
    let ParseMeshAttribute (line: string) =
        let prefixLength = line.IndexOf ' '
        let prefix, suffix = line.Substring(0, prefixLength), line.Substring(prefixLength + 1)
        match prefix with
        | "v" ->
            suffix.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map System.Single.Parse
            |> (fun xyz -> Vector3(ReadOnlySpan xyz))
            |> Position
        | "vn" ->
            suffix.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map System.Single.Parse
            |> (fun xyz -> Vector3(ReadOnlySpan xyz))
            |> Normal
        | "vt" ->
            suffix.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map System.Single.Parse
            |> (fun xy -> Vector2(ReadOnlySpan xy))
            |> TexCoord
        | "f" ->
            suffix.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Seq.ofArray
            |> Seq.map (fun str ->
                let components = str.Split('/')
                let positionIdx = System.Int32.Parse components[0]
                let normalIdx = if components.Length = 3 && not (String.IsNullOrEmpty(components[2])) then System.Int32.Parse components[2] else 0
                let texcoordIdx = if components.Length >= 2 && not (String.IsNullOrEmpty(components[1])) then System.Int32.Parse components[1] else 0
                {
                    positionIdx = positionIdx
                    normalIdx = normalIdx
                    texcoordIdx = texcoordIdx
                }
            )
            |> List.ofSeq
            |> Face
        | _ -> failwith "Unknown geometry attribute!"
    let ParseOBJ (filename: string) =
        let isGeometryData (str: string) =
            str.StartsWith("v ") || str.StartsWith("f ") || str.StartsWith("vt ") || str.StartsWith("vn ")
        let _, _, _, vertices, triangles, _ =
            query {
                for line in File.ReadLines filename do
                where (isGeometryData line)
                select (ParseMeshAttribute (line.Trim()))
            }
            |> Seq.fold (fun (positions: ResizeArray<Vector3>, normals: ResizeArray<Vector3>, texcoords: ResizeArray<Vector2>, vertices: ResizeArray<Vertex>, triangles: ResizeArray<int>, vertexDictionary: Dictionary<Index,int>) attribute ->
                match attribute with
                | Position position -> positions.Add position
                | Normal normal -> normals.Add normal
                | TexCoord texcoord -> texcoords.Add texcoord
                | Face indices ->
                    indices
                    |> Seq.ofList
                    |> Seq.map (fun index ->
                        let index = {
                            positionIdx = if index.positionIdx <= 0 then index.positionIdx + positions.Count else index.positionIdx - 1
                            normalIdx = if index.normalIdx <= 0 then index.normalIdx + normals.Count else index.normalIdx - 1
                            texcoordIdx = if index.texcoordIdx <= 0 then index.texcoordIdx + texcoords.Count else index.texcoordIdx - 1
                        }
                        if vertexDictionary.TryAdd(index, vertices.Count) then
                            vertices.Add {
                                position = positions[index.positionIdx]
                                normal = if index.normalIdx < normals.Count then normals[index.normalIdx] else Vector3.UnitY
                                texcoord = if index.texcoordIdx < texcoords.Count then texcoords[index.texcoordIdx] else Vector2.Zero
                            }
                            vertices.Count - 1
                        else
                            vertexDictionary.Item index
                    )
                    |> List.ofSeq
                    |> (function 
                        | i :: tail ->
                            tail
                            |> Seq.ofList
                            |> Seq.pairwise
                            |> Seq.iter (fun (j, k) ->
                                triangles.Add i
                                triangles.Add j
                                triangles.Add k
                            )
                        | _ -> ()
                    )
                positions, normals, texcoords, vertices, triangles, vertexDictionary
            ) (ResizeArray(), ResizeArray(), ResizeArray(), ResizeArray(), ResizeArray(), Dictionary())            
        { vertices = [| for vertex in vertices -> vertex |]; triangles = [| for index in triangles -> index |] }
module Processor =
    let RecalculateNormal mesh =
        let normals = Span (NativePtr.toVoidPtr (NativePtr.stackalloc<Vector3> mesh.vertices.Length), mesh.vertices.Length)
        for n in 0 .. mesh.triangles.Length / 3 - 1 do
            let i, j, k = mesh.triangles[3 * n], mesh.triangles[3 * n + 1], mesh.triangles[3 * n + 2]
            let p1, p2, p3 = mesh.vertices[i].position, mesh.vertices[j].position, mesh.vertices[k].position
            let normal = Vector3.Normalize (Vector3.Cross(p2 - p1, p3 - p1))
            let w1 = acos(Vector3.Dot(Vector3.Normalize(p2 - p1), Vector3.Normalize(p3 - p1)))
            let w2 = acos(Vector3.Dot(Vector3.Normalize(p3 - p2), Vector3.Normalize(p1 - p2)))
            normals[i] <- normals[i] + normal * w1
            normals[j] <- normals[j] + normal * w2
            normals[k] <- normals[k] + normal * (MathF.PI - w1 - w2)
        let mutable vertices = Array.zeroCreate mesh.vertices.Length
        for i in 0 .. vertices.Length - 1 do
            vertices[i] <- { mesh.vertices[i] with normal = Vector3.Normalize normals[i] }
        { mesh with vertices = vertices }
