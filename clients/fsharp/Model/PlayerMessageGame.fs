namespace AiCup2019.Model

type PlayerMessageType = CustomDataMessage = 0 | ActionMessage = 1

module PlayerMessageGame =
    [<AbstractClass>]
    type T() =
        abstract member writeTo: System.IO.BinaryWriter -> unit

module CustomDataMessage =
    type T(customData) =
        inherit PlayerMessageGame.T()
        member this.CustomData : CustomData.T = customData

        override this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write (int PlayerMessageType.CustomDataMessage)
            this.CustomData.writeTo writer

    let readFrom (reader: System.IO.BinaryReader) =
        new T(CustomData.T.readFrom reader)

module ActionMessage =
    type T(action) =
        inherit PlayerMessageGame.T()
        member this.Action : Map<int, UnitAction.T> = action

        override this.writeTo (writer: System.IO.BinaryWriter) =
            writer.Write (int PlayerMessageType.ActionMessage)
            writer.Write this.Action.Count
            this.Action |> Map.iter(fun k v -> writer.Write k
                                               v.writeTo writer)

    let readFrom (reader: System.IO.BinaryReader) = 
        new T([for _ = 1 to reader.ReadInt32() do
                    let k = reader.ReadInt32()
                    let v = UnitAction.readFrom reader
                    yield (k,v)]
              |> Map.ofList)

[<AutoOpen>]
module PlayerMessageGameExtensions =
    type PlayerMessageGame.T with
        static member readFrom (reader: System.IO.BinaryReader) =
            match reader.ReadInt32() |> enum with
                | PlayerMessageType.CustomDataMessage -> CustomDataMessage.readFrom reader 
                                                         :> PlayerMessageGame.T
                | PlayerMessageType.ActionMessage -> ActionMessage.readFrom reader 
                                                         :> PlayerMessageGame.T
                | x -> failwith (sprintf "Unexpected PlayerMessageType %d" (int x))
