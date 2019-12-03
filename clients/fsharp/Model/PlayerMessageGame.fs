#nowarn "0058"
namespace AiCup2019.Model

type PlayerMessageGameCustomDataMessage = {
    Data: CustomData;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 0
        this.Data.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) = {
        Data = CustomData.readFrom reader
    }

type PlayerMessageGameActionMessage = {
    Action: Versioned;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 1
        this.Action.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) = {
        Action = Versioned.readFrom reader
    }
type PlayerMessageGame = 
    | CustomDataMessage of PlayerMessageGameCustomDataMessage
    | ActionMessage of PlayerMessageGameActionMessage
    with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this with
            | CustomDataMessage value -> value.writeTo writer
            | ActionMessage value -> value.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) =
        match reader.ReadInt32() with
            | 0 -> CustomDataMessage (PlayerMessageGameCustomDataMessage.readFrom reader)
            | 1 -> ActionMessage (PlayerMessageGameActionMessage.readFrom reader)
            | x -> failwith (sprintf "Unexpected CustomDataType %d" x)
