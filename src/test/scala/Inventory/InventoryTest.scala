package Inventory

import Items.{ItemManager, ItemRef}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

class InventoryTest extends AnyFlatSpec {
  "인벤토리 매니저" should "새로운 아이템 추가 시, 요소 추가" in {
    val inventory = new InventoryRef(mutable.Map[Int, InvenUnit]())
    inventory.addItem(ItemManager.createItem(1).get)
    assert(inventory.inventory.size == 1)
    assert(inventory.inventory(1).count == 1)
    inventory.addItem(ItemManager.createItem(2).get)
    assert(inventory.inventory.size == 2)
    assert(inventory.inventory(2).count == 1)
  }

  "인벤토리 매니저" should "기존 아이템 추가 시, 카운트 증가" in {
    val inventory = new InventoryRef(mutable.Map[Int, InvenUnit]())
    inventory.addItem(ItemManager.createItem(1).get)
    inventory.addItem(ItemManager.createItem(1).get)
    assert(inventory.inventory.size == 1)
    assert(inventory.inventory(1).count == 2)
  }

  "인벤토리 매니저" should "InvenUnit 으로 아이템 추가 가능 " in {
    val inventory = new InventoryRef(mutable.Map[Int, InvenUnit]())
    inventory.addItem(InvenUnit(1, 10, 10))
    assert(inventory.inventory.size == 1)
    assert(inventory.inventory(1).count == 10)
  }

  "인벤토리 매니저" should "인벤토리 max slot 체크" in {
    val inventory = new InventoryRef(mutable.Map[Int, InvenUnit]())
    inventory.addItem(InvenUnit(1, 10, 2))
    inventory.addItem(InvenUnit(2, 10, 3))
    inventory.addItem(InvenUnit(3, 10, 2))

    assert(inventory.inventory.size == 2)
  }

  "인벤토리 매니저" should "인벤토리 삭제 체크" in {
    val inventory = new InventoryRef(mutable.Map[Int, InvenUnit]())
    inventory.addItem(InvenUnit(1, 10, 2))
    inventory.delItem(InvenUnit(1, 5, 2))

    assert(inventory.inventory(1).count == 5)
  }

  "인벤토리 매니저" should "전량 삭제 시, 슬롯에서 빠짐 체크 " in {
    val inventory = new InventoryRef(mutable.Map[Int, InvenUnit]())
    inventory.addItem(InvenUnit(1, 10, 2))
    inventory.delItem(InvenUnit(1, 10, 2))

    assert(inventory.inventory.isEmpty)
  }
}
