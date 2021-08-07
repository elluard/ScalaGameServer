package Inventory

import Inventory.InventoryManager.Inventory
import Items.{ItemManager, ItemRef}
import org.scalatest.flatspec.AnyFlatSpec

import cats.instances.either

class InventoryTest extends AnyFlatSpec {
  "인벤토리 매니저" should "새로운 아이템 추가 시, 요소 추가" in {
    val inventory : Inventory = Map[Int, InvenUnit]()
    val result1 = InventoryManager.addItem(inventory, ItemManager.createItem(1).get, 10)
      .flatMap { inven =>
        InventoryManager.addItem(inven, ItemManager.createItem(2).get, 10)
      }
    assert(result1.isRight)
    result1.map { inven =>
      assert(inven(1).count == 1)
      assert(inven(2).count == 1)
    }
  }

  "인벤토리 매니저" should "기존 아이템 추가 시, 카운트 증가" in {
    val inventory : Inventory = Map[Int, InvenUnit]()
    val result = InventoryManager.addItem(inventory, ItemManager.createItem(1).get, 10)
      .flatMap{ inven =>
        InventoryManager.addItem(inven, ItemManager.createItem(1).get, 10)
      }
    assert(result.isRight)

    result.map { inven =>
      assert(inven(1).count == 2)
    }
  }

  "인벤토리 매니저" should "InvenUnit 으로 아이템 추가 가능 " in {
    val inventory : Inventory = Map[Int, InvenUnit]()
    val result = InventoryManager.addItem(inventory, InvenUnit(1, 10, 5), 10)
      .flatMap{ inven =>
        InventoryManager.addItem(inven, InvenUnit(1, 10, 5), 10)
      }

    assert(result.isRight)

    result.map { inven =>
      assert(inven(1).count == 20)
    }
  }

  "인벤토리 매니저" should "인벤토리 max slot 체크 - InvenUnit" in {
    val inventory : Inventory = Map[Int, InvenUnit]()

    val result = InventoryManager.addItem(inventory, InvenUnit(1, 8, 2), 5)
      .flatMap{ inven =>
        InventoryManager.addItem(inven, InvenUnit(2, 8, 2), 5)
      }

    assert(result.isLeft)
  }

  "인벤토리 매니저" should "인벤토리 max slot 체크 - ItemRef" in {
    val inventory : Inventory = Map[Int, InvenUnit]()

    val result = InventoryManager.addItem(inventory, InvenUnit(1, 10, 2), 5)
      .flatMap{ inven =>
        InventoryManager.addItem(inven, InvenUnit(2, 8, 2), 5)
      }

    assert(result.isLeft)
  }

  "인벤토리 매니저" should "인벤토리 삭제 체크" in {
    val inventory : Inventory = Map[Int, InvenUnit]()
    val result = InventoryManager.addItem(inventory, InvenUnit(1, 10, 2), 5)
      .flatMap{ inven =>
        InventoryManager.delItem(inven, InvenUnit(1, 1, 2))
      }
    assert(result.isRight)
    result.map{ inven =>
      assert(inven(1).count == 9)
    }
  }

  "인벤토리 매니저" should "전량 삭제 시, 슬롯에서 빠짐 체크 " in {
    val inventory : Inventory = Map[Int, InvenUnit]()
    val result = InventoryManager.addItem(inventory, InvenUnit(1, 10, 2), 10)
      .flatMap{ inven =>
        InventoryManager.addItem(inven, InvenUnit(2, 10, 2), 10)
      }
      .flatMap { inven =>
        InventoryManager.delItem(inven, InvenUnit(1, 1, 2))
      }
      .flatMap { inven =>
        InventoryManager.delItem(inven, InvenUnit(1, 9, 2))
      }
    assert(result.isRight)
    result.map{ inven =>
      assert(!inven.contains(1))
    }
  }

  "인벤토리 매니저" should " 한도초과 시, 에러 메시지 출력 " in {
    val inventory : Inventory = Map[Int, InvenUnit]()
    val result = InventoryManager.addItem(inventory, InvenUnit(1, 10, 2), 5)
      .flatMap { inven =>
        InventoryManager.delItem(inven, InvenUnit(1, 11, 2))
      }
    assert(result.isLeft)
    result.left.map { a=>
      assert(a == "Not enough Item")
    }
  }

  "인벤토리 매니저" should " 없는 아이템 삭제 시, 에러 메시지 출력 " in {
    val inventory : Inventory = Map[Int, InvenUnit]()
    val result = InventoryManager.addItem(inventory, InvenUnit(1, 10, 2), 5)
      .flatMap { inven =>
        InventoryManager.delItem(inven, InvenUnit(2, 1, 2))
      }
    assert(result.isLeft)
    result.left.map { a=>
      assert(a == "Item not found")
    }
  }
}
