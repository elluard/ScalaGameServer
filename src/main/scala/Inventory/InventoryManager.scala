package Inventory

import Items._

import cats.MonadError
import cats.instances.either
import cats.syntax.either._

case class InvenUnit(itemId : Int, count : Int, maxStack: Int)

object InventoryManager {
  type Inventory = Map[Int, InvenUnit]

  def addItem(inventory: Inventory, item: ItemRef, slotCount: Int): Either[String, Inventory] = {
    val inven = inventory.getOrElse(item.id, InvenUnit(item.id, 0, item.maxStack))
    val result = inventory.updated(item.id, InvenUnit(item.id, inven.count + 1, item.maxStack))

    if (slotCount < usingCount(result)) {
      "slot is full".asLeft
    }
    else result.asRight
  }

  def addItem(inventory: Inventory, item: InvenUnit, slotCount: Int): Either[String, Inventory] = {
    val inven = inventory.getOrElse(item.itemId, InvenUnit(item.itemId, 0, item.maxStack))
    val result = inventory.updated(inven.itemId, InvenUnit(inven.itemId, inven.count + item.count, item.maxStack))

    if (slotCount < usingCount(result)) {
      "slot is full".asLeft
    }
    else result.asRight
  }

  def delItem(inventory: Inventory, invenUnit: InvenUnit): Either[String, Inventory] = {
    val result = inventory.get(invenUnit.itemId) match {
      case Some(item) if invenUnit.count <= item.count => InvenUnit(item.itemId, item.count - invenUnit.count, item.maxStack).asRight
      case Some(_) => "Not enough Item".asLeft
      case _ => "Item not found".asLeft
    }

    result.map { unit =>
      if (unit.count < 1) {
        inventory - unit.itemId
      }
      else {
        inventory.updated(unit.itemId, unit)
      }
    }
  }

  def usingCount(inventory: Inventory): Int =
    inventory.foldLeft(0) { (acc, curr) =>
      acc + ((curr._2.count - 1) / curr._2.maxStack + 1)
    }
}
