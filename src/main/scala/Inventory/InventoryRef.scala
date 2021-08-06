package Inventory

import Items._

import scala.collection.mutable

case class InvenUnit (itemId : Int, count : Int)

class InventoryRef(var inventory : mutable.IndexedSeq[InvenUnit]) {
  def addItem(item : ItemRef) : Unit = {
    val index = inventory.indexWhere{ inven =>
      inven.itemId == item.id && inven.count < item.maxStack
    }

    if(index == -1) {
      this.inventory = this.inventory :+ InvenUnit(itemId = item.id, count = 1)
    }
    else {
      val tempInven = inventory(index)
      inventory.update(index, InvenUnit(tempInven.itemId, tempInven.count + 1))
    }
  }

  def addItem(invenUnit : InvenUnit) : Unit = {
    this.inventory = this.inventory :+ invenUnit
  }
}
