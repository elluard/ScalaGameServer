package Cookie

import cats.syntax.either._

final case class Cookie(id: Int, level: Int, exp: Int, grade : Int, skillLevel: Int, soulStone : Int)

object CookieManager {
  private val upgradeMap = Map[Int, Int] (
    1 -> 20,
    2 -> 30,
    3 -> 50,
    4 -> 70,
    5 -> 100
  )
  def expUp(cookie : Cookie, exp : Int) : Cookie = {
    // TODO : 차후에 레벨별 테이블 완성되면 세팅 후 사용토록 수정
    if(cookie.exp + exp > 100) {
      Cookie(cookie.id, cookie.level + 1, (cookie.exp + exp) - 100, cookie.grade, cookie.skillLevel, cookie.soulStone)
    }
    else {
      Cookie(cookie.id, cookie.level, cookie.exp + exp, cookie.grade, cookie.skillLevel, cookie.soulStone)
    }
  }

  def skillLevelUp(cookie: Cookie, lvCount : Int) : Cookie = {
    Cookie(cookie.id, cookie.level, cookie.exp, cookie.grade, cookie.skillLevel + lvCount, cookie.soulStone)
  }

  def upgrade(cookie : Cookie) : Either[String, Cookie] = {
    Either
      .fromOption(upgradeMap.get(cookie.grade + 1), { s"Invalid Upgrade grade ${cookie.grade}"})
      .flatMap{ reqStone =>
        if(cookie.soulStone < reqStone) {
          "Not enough soulstrong".asLeft
        }
        else {
          Cookie(cookie.id, cookie.level, cookie.exp, cookie.grade + 1, cookie.skillLevel, cookie.soulStone - reqStone).asRight
        }
      }
  }
}
