#ifndef _MODEL_SERVER_MESSAGE_GAME_HPP_
#define _MODEL_SERVER_MESSAGE_GAME_HPP_

#include "../Stream.hpp"
#include <string>
#include <memory>
#include <stdexcept>
#include "PlayerView.hpp"
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

class ServerMessageGame {
public:
    std::shared_ptr<PlayerView> playerView;
    ServerMessageGame();
    ServerMessageGame(std::shared_ptr<PlayerView> playerView);
    static ServerMessageGame readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
