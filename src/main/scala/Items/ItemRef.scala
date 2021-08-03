package Items

case class ItemRef(id: Int, name : String, category1: String, category2: String, sellable: Boolean)

//TODO : 한번에 모든 아이템을 다 만들기는 쉽지 않다, 기본틀만 잡아두고, 다른 컨텐츠 구현 할 때, 필요한 아이템을 추가하도록 하자.
object ItemManager {
  def createItem(id : Int): Option[ItemRef] =  {
    id match {
      case 1 => Some(ItemRef(1, "혼돈의 케이크 파편", "소모품", "혼돈의 케이크 파편",  sellable = true))
      case _ => None
    }
  }
}