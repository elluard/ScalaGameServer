package Items

case class ItemRef(id: Int, name : String, category1: String, category2: String, sellable: Boolean, maxStack : Int = 30)

//TODO : 한번에 모든 아이템을 다 만들기는 쉽지 않다, 기본틀만 잡아두고, 다른 컨텐츠 구현 할 때, 필요한 아이템을 추가하도록 하자.
object ItemManager {
  def createItem(id : Int): Option[ItemRef] =  {
    id match {
      case 1 => Some(ItemRef(1, "혼돈의 케이크 파편", "소모품", "혼돈의 케이크 파편",  sellable = true, maxStack = 999))
      case 2 => Some(ItemRef(2, "경험의 별사탕 레벨 1", "소모품", "경험의 별사탕",  sellable = true, maxStack = 999))
      case 3 => Some(ItemRef(3, "경험의 별사탕 레벨 2", "소모품", "경험의 별사탕",  sellable = true, maxStack = 999))
      case 4 => Some(ItemRef(4, "경험의 별사탕 레벨 3", "소모품", "경험의 별사탕",  sellable = true, maxStack = 999))
      case 5 => Some(ItemRef(5, "경험의 별사탕 레벨 4", "소모품", "경험의 별사탕",  sellable = true, maxStack = 999))
      case _ => None
    }
  }
}