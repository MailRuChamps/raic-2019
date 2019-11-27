#ifndef _MODEL_GAME_HPP_
#define _MODEL_GAME_HPP_

#include "../Stream.hpp"
#include <string>
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

class Game {
public:
    int currentTick;
    Properties properties;
    Level level;
    std::vector<Player> players;
    std::vector<Unit> units;
    std::vector<Bullet> bullets;
    std::vector<Mine> mines;
    std::vector<LootBox> lootBoxes;
    Game();
    Game(int currentTick, Properties properties, Level level, std::vector<Player> players, std::vector<Unit> units, std::vector<Bullet> bullets, std::vector<Mine> mines, std::vector<LootBox> lootBoxes);
    static Game readFrom(InputStream& stream);
    void writeTo(OutputStream& stream) const;
    std::string toString() const;
};

#endif
