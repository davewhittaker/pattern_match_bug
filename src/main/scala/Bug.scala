import net.liftweb.json._
import net.liftweb.common._
import java.text._
import java.util._

object Bug extends Application {

  val date = new SimpleDateFormat("YYYY-MM-dd hh:mm:ss")

  def extractAsset(asset: JValue): Box[Asset] = asset match {
    case obj: JObject =>
      (obj \ "asset_id", 
       obj \ "filename", 
       obj \ "property_id", 
       obj \ "subject_id", 
       obj \ "date_created", 
       obj \ "last_modified", 
       obj \ "thumbnail_url", 
       obj \ "medium_url") match {
        case (JInt(id),
              JString(fileName),
              JInt(propertyId),
              JString(subjectId),
              JString(dateCreated),
              JString(lastModified),
              JString(thumbnailUrl),
              JString(mediumUrl)) =>
                Full(
                        Asset(
                                id = id.toInt,
                                fileName = fileName,
                                propertyId = propertyId.toInt,
                                subjectId = subjectId,
                                dateCreated = date.parse(dateCreated),
                                lastModified = date.parse(lastModified),
                                thumbnailUrl = thumbnailUrl,
                                mediumUrl = mediumUrl))
        case other =>
          Failure("Response did not contain all of the required fields, or at least one field was of the wrong type %s".format(obj))
      }
    case other =>
      Failure("Non JObject found where an asset representation was expected : %s".format(asset))
  }


  case class Asset(
    id: Int,
    fileName: String,
    propertyId: Int,
    subjectId: String,
    dateCreated: Date,
    lastModified: Date,
    thumbnailUrl: String,
    mediumUrl: String)

}
