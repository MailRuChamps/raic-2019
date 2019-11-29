namespace AiCup2019.Model

type PlayerMessageType = CustomDataMessage = 0 | ActionMessage = 1

type CustomDataMessage = {CustomData: CustomData.T} with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write (int PlayerMessageType.CustomDataMessage)
        this.CustomData.writeTo writer

    static member readFrom (reader: System.IO.BinaryReader) =
        {CustomData = CustomData.T.readFrom reader}

type ActionMessage = {Action: Map<int, UnitAction>} with
    member this.writeTo (writer: System.IO.BinaryWriter) =
        writer.Write (int PlayerMessageType.ActionMessage)
        writer.Write this.Action.Count
        this.Action |> Map.iter(fun k v -> writer.Write k
                                           v.writeTo writer)
    static member readFrom (reader: System.IO.BinaryReader) = 
            { Action = [for _ = 1 to reader.ReadInt32() do
                        let k = reader.ReadInt32()
                        let v = UnitAction.readFrom reader
                        yield (k,v)]
                    |> Map.ofList }

type PlayerMessageGame = ActionMessage of ActionMessage| CustomDataMessage of CustomDataMessage with
    member this.writeTo (writer: System.IO.BinaryWriter) =
              match this with
                  | ActionMessage x -> x.writeTo writer
                  | CustomDataMessage x -> x.writeTo writer
    static member readFrom (reader: System.IO.BinaryReader) =
        match reader.ReadInt32() |> enum with
            | PlayerMessageType.CustomDataMessage -> CustomDataMessage (CustomDataMessage.readFrom reader)
            | PlayerMessageType.ActionMessage -> ActionMessage (ActionMessage.readFrom reader)
            | x -> failwith (sprintf "Unexpected PlayerMessageType %d" (int x))
