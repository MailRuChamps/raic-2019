namespace AiCup2019

open AiCup2019.Model
module MyStrategy =
    type T() =
        static member DistanceSqr (a: Vec2Double.T, b: Vec2Double.T) = 
            (a.X - b.X) * (a.X - b.X) + (a.Y - b.Y) * (a.Y - b.Y)
            
        member this.getAction(unit: Unit.T, game: Game.T, debug: Debug.T) =
            let nearestEnemy = game.Units |> Array.filter(fun u -> u.PlayerId <> unit.PlayerId)
                                          |> Array.sortBy(fun u -> T.DistanceSqr(u.Position, unit.Position))
                                          |> Seq.tryFind(fun _ -> true)

            let nearestWeapon = game.LootBoxes |> Array.choose(fun b -> match b.Item with
                                                                            | :? Item.WeaponItem -> Some b
                                                                            | _ -> None)
                                                |> Array.sortBy(fun b -> T.DistanceSqr(b.Position, unit.Position))
                                                |> Seq.tryFind(fun _ -> true)

            let mutable targetPos = unit.Position

            if not unit.Weapon.IsSome && nearestWeapon.IsSome then
                targetPos <- nearestWeapon.Value.Position
            else if nearestEnemy.IsSome then
                targetPos <- nearestEnemy.Value.Position

            debug.draw(new CustomData.Log(sprintf "Target pos: %A" targetPos))

            let aim: Vec2Double.T = match nearestEnemy with
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
                SwapWeapon = false
                PlantMine = false
            }          
    

