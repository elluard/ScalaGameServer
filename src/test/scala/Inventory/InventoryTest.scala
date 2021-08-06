package Inventory

import Items.{ItemManager, ItemRef}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

class InventoryTest extends AnyFlatSpec {
  "인벤토리 매니저" should "새로운 아이템 추가 시, 요소 추가" in {
    val inventory = new InventoryRef(mutable.IndexedSeq[InvenUnit]())
    inventory.addItem(ItemManager.createItem(1).get)
    assert(inventory.inventory.length == 1)
    inventory.addItem(ItemManager.createItem(2).get)
    assert(inventory.inventory.length == 2)
  }

  "인벤토리 매니저" should "기존 아이템 추가 시, 카운트 증가" in {
    val inventory = new InventoryRef(mutable.IndexedSeq[InvenUnit]())
    inventory.addItem(ItemManager.createItem(1).get)
    inventory.addItem(ItemManager.createItem(1).get)
    assert(inventory.inventory.length == 1)
    assert(inventory.inventory(0).count == 2)
  }

  "인벤토리 매니저" should "기존 아이템 max stack 값 초과시, 새로 추가" in {
    val inventory = new InventoryRef(mutable.IndexedSeq[InvenUnit]())
    for(x <- 0 to 998) {
      inventory.addItem(ItemManager.createItem(1).get)
    }

    assert(inventory.inventory.length === 1)
    assert(inventory.inventory(0).count === 999)

    inventory.addItem(ItemManager.createItem(1).get)

    assert(inventory.inventory.length === 2)
    assert(inventory.inventory(1).count === 1)
  }
}
