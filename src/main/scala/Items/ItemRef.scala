package Items

case class ItemRef(id: Int, name : String, category1: String, category2: String, sellable: Boolean)

object ItemManager {
  def createItem(id : Int): Option[ItemRef] =  {
    id match {
      case 1 => Some(ItemRef(1, "혼돈의 케이크 파편", "소모품", "혼돈의 케이크 파편",  sellable = true))
      case 2 => Some(ItemRef(2, "경험의 별사탕 레벨1", "소모품", "경험의 별사탕",  sellable = true))
      case _ => None
    }
  }
}