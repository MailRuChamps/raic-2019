namespace AiCup2019.Model

module ServerMessageGame =
    type T = {PlayerView: option<PlayerView.T>} with
        member this.writeTo (writer: System.IO.BinaryWriter) =
            match this.PlayerView with
                | Some x -> writer.Write true
                            x.writeTo writer
                | None -> writer.Write false
    
    let readFrom (reader: System.IO.BinaryReader) =
        match reader.ReadBoolean() with
            | true -> Some(PlayerView.readFrom reader)
            | _ -> None     

