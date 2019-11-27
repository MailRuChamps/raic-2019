#ifndef _MODEL_PLAYER_VIEW_HPP_
#define _MODEL_PLAYER_VIEW_HPP_

#include "../Stream.hpp"
#include <string>
#include <stdexcept>
#include "Game.hpp"
#include <stdexcept>
#include "Properties.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <unordered_map>
#include <stdexcept>
#include "WeaponType.hpp"
#include <stdexcept>
#include "WeaponParams.hpp"
#include <stdexcept>
#include "BulletParams.hpp"
#include <memory>
#include <stdexcept>
#include "ExplosionParams.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "ExplosionParams.hpp"
#include <stdexcept>
#include "Level.hpp"
#include <vector>
#include <vector>
#include <stdexcept>
#include "Tile.hpp"
#include <vector>
#include <stdexcept>
#include "Player.hpp"
#include <vector>
#include <stdexcept>
#include "Unit.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "JumpState.hpp"
#include <memory>
#include <stdexcept>
#include "Weapon.hpp"
#include <stdexcept>
#include "WeaponType.hpp"
#include <stdexcept>
#include "WeaponParams.hpp"
#include <stdexcept>
#include "BulletParams.hpp"
#include <memory>
#include <stdexcept>
#include "ExplosionParams.hpp"
#include <memory>
#include <memory>
#include <memory>
#include <vector>
#include <stdexcept>
#include "Bullet.hpp"
#include <stdexcept>
#include "WeaponType.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <memory>
#include <stdexcept>
#include "ExplosionParams.hpp"
#include <vector>
#include <stdexcept>
#include "Mine.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "MineState.hpp"
#include <memory>
#include <stdexcept>
#include "ExplosionParams.hpp"
#include <vector>
#include <stdexcept>
#include "LootBox.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Vec2Double.hpp"
#include <stdexcept>
#include "Item.hpp"
#include <stdexcept>
#include "WeaponType.hpp"

class PlayerView {
public:
    int myId;
    Game game;
    PlayerView();
    PlayerView(int myId, Game game);
    static PlayerView readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
