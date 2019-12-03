namespace AiCup2019

open AiCup2019.Model

type MyStrategy() =
    static member DistanceSqr (a: Vec2Double, b: Vec2Double) = 
        (a.X - b.X) * (a.X - b.X) + (a.Y - b.Y) * (a.Y - b.Y)
            
    member this.getAction(unit: Unit, game: Game, debug: Debug) =
        let nearestEnemy = game.Units |> Array.filter(fun u -> u.PlayerId <> unit.PlayerId)
                                        |> Array.sortBy(fun u -> MyStrategy.DistanceSqr(u.Position, unit.Position))
                                        |> Seq.tryFind(fun _ -> true)

        let nearestWeapon = game.LootBoxes |> Array.choose(fun b -> match b.Item with
                                                                        | Item.Weapon _ -> Some b.Position
                                                                        | _ -> None)
                                            |> Array.sortBy(fun p -> MyStrategy.DistanceSqr(p, unit.Position))
                                            |> Seq.tryFind(fun _ -> true)

        let mutable targetPos = unit.Position

        if not unit.Weapon.IsSome && nearestWeapon.IsSome then
            targetPos <- nearestWeapon.Value
        else if nearestEnemy.IsSome then
            targetPos <- nearestEnemy.Value.Position

        debug.draw(CustomData.Log {Text = sprintf "Target pos: %A" targetPos })

        let aim: Vec2Double = match nearestEnemy with
                                | Some x -> { X = x.Position.X - unit.Position.X; Y = x.Position.Y - unit.Position.Y} 
                                | None -> { X = 0.0; Y = 0.0 }

        let mutable jump = targetPos.Y > unit.Position.Y

        if targetPos.X > unit.Position.X && game.Level.Tiles.[(int unit.Position.X + 1)].[(int unit.Position.Y)] = Tile.Wall then jump <- true
        if targetPos.X < unit.Position.X && game.Level.Tiles.[(int unit.Position.X - 1)].[(int unit.Position.Y)] = Tile.Wall then jump <- true
                        
        {
            Velocity = targetPos.X - unit.Position.X
            Jump = jump
            JumpDown = not jump
            Aim = aim
            Shoot = true
            Reload = false
            SwapWeapon = false
            PlantMine = false
        }          
