package model

import util.StreamUtil

class Unit {
    var playerId: Int = 0
    var id: Int = 0
    var health: Int = 0
    lateinit var position: model.Vec2Double
    lateinit var size: model.Vec2Double
    lateinit var jumpState: model.JumpState
    var walkedRight: Boolean = false
    var stand: Boolean = false
    var onGround: Boolean = false
    var onLadder: Boolean = false
    var mines: Int = 0
    var weapon: model.Weapon? = null
    constructor() {}
    constructor(playerId: Int, id: Int, health: Int, position: model.Vec2Double, size: model.Vec2Double, jumpState: model.JumpState, walkedRight: Boolean, stand: Boolean, onGround: Boolean, onLadder: Boolean, mines: Int, weapon: model.Weapon?) {
        this.playerId = playerId
        this.id = id
        this.health = health
        this.position = position
        this.size = size
        this.jumpState = jumpState
        this.walkedRight = walkedRight
        this.stand = stand
        this.onGround = onGround
        this.onLadder = onLadder
        this.mines = mines
        this.weapon = weapon
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Unit {
            val result = Unit()
            result.playerId = StreamUtil.readInt(stream)
            result.id = StreamUtil.readInt(stream)
            result.health = StreamUtil.readInt(stream)
            result.position = model.Vec2Double.readFrom(stream)
            result.size = model.Vec2Double.readFrom(stream)
            result.jumpState = model.JumpState.readFrom(stream)
            result.walkedRight = StreamUtil.readBoolean(stream)
            result.stand = StreamUtil.readBoolean(stream)
            result.onGround = StreamUtil.readBoolean(stream)
            result.onLadder = StreamUtil.readBoolean(stream)
            result.mines = StreamUtil.readInt(stream)
            if (StreamUtil.readBoolean(stream)) {
                result.weapon = model.Weapon.readFrom(stream)
            } else {
                result.weapon = null
            }
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, playerId)
        StreamUtil.writeInt(stream, id)
        StreamUtil.writeInt(stream, health)
        position.writeTo(stream)
        size.writeTo(stream)
        jumpState.writeTo(stream)
        StreamUtil.writeBoolean(stream, walkedRight)
        StreamUtil.writeBoolean(stream, stand)
        StreamUtil.writeBoolean(stream, onGround)
        StreamUtil.writeBoolean(stream, onLadder)
        StreamUtil.writeInt(stream, mines)
        val weapon = weapon;
        if (weapon == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            weapon.writeTo(stream)
        }
    }
}
