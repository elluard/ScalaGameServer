package Cookie

import org.scalatest.flatspec.AnyFlatSpec

class CookieTest extends AnyFlatSpec {
  private val cookie = Cookie(1, 1, 20, 1, 1, 40)
  "쿠키 매니저" should "경험치 증가, 결과 값 확인" in {
    val newCookie = CookieManager.expUp(cookie, 10)

    assert(newCookie.exp == 30)
  }

  "쿠키 매니저" should "경험치 증가 후 레벨업, 결과 값 확인" in {
    val newCookie = CookieManager.expUp(cookie, 90)

    assert(newCookie.level == 2)
    assert(newCookie.exp == 10)
  }

  "쿠키 매니저" should "단계 증가 확인" in {
    val newCookie = CookieManager.upgrade(cookie)
    assert(newCookie.isRight)
    newCookie.map { cookie =>
      assert(cookie.grade == 2)
      assert(cookie.soulStone == 10)
    }
  }

  "쿠키 매니저" should "단계상승 시 영혼석 모자람, 실패" in {
    val tempCookie = Cookie(1, 1, 20, 0, 1, 10)

    val newCookie = CookieManager.upgrade(tempCookie)
    assert(newCookie.isLeft)
  }

  "쿠키 매니저" should "스킬레벨 상승" in {
    val newCookie = CookieManager.skillLevelUp(cookie, 1)
    assert(newCookie.skillLevel == 2)
  }
}
