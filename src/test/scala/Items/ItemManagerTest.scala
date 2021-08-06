package Items

import org.scalatest.flatspec.AnyFlatSpec

class ItemManagerTest extends AnyFlatSpec {
  "ItemManger" should "create item when valid input" in {
    val item = ItemManager.createItem(1)
    assert(item.isDefined)
    assert(item.get.id === 1)
  }

  "ItemManager" should "return None when invalid input" in {
    val item = ItemManager.createItem(-1)
    assert(item.isEmpty)
  }
}
