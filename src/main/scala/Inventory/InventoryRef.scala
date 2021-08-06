package Inventory

import Items._

import scala.collection.mutable

case class InvenUnit(itemId : Int, count : Int, maxStack: Int)

class InventoryRef(var inventory : mutable.Map[Int/*ItemID*/, InvenUnit]) {
  var maxCount = 5

  def usingCount : Int =
    inventory.foldLeft(0){(acc, curr)=>
      acc + ((curr._2.count - 1) / curr._2.maxStack + 1)
    }

  def addItem(item : ItemRef) : Unit = {
    val tempInven = inventory.clone()
    val inven = tempInven.getOrElse(item.id, InvenUnit(item.id, 0, item.maxStack))
    tempInven.update(item.id, InvenUnit(item.id, inven.count + 1, item.maxStack))

    //허용된 슬롯 갯수를 체크하기위한 count 계산
    if(usingCount <= maxCount) {
      inventory = tempInven
    }
  }

  def addItem(item : InvenUnit) : Unit = {
    val tempInven = inventory.clone()
    val inven = tempInven.getOrElse(item.itemId, InvenUnit(item.itemId, 0, item.maxStack))
    tempInven.update(inven.itemId, InvenUnit(inven.itemId, inven.count + item.count, item.maxStack))

    if(usingCount <= maxCount) {
      inventory = tempInven
    }
  }

  def delItem(invenUnit: InvenUnit) : Unit = {
    val result = inventory.get(invenUnit.itemId) match {
      case Some(item) if invenUnit.count <= item.count => Some(InvenUnit(item.itemId, item.count - invenUnit.count, item.maxStack))
      case _ => None
    }

    result.foreach{ unit =>
      if(unit.count < 1) {
        inventory.remove(unit.itemId)
      }
      else {
        inventory.update(unit.itemId, unit)
      }
    }
  }
}
