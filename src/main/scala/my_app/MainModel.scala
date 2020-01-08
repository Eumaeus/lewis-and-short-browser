package citeLexicon 
import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext.Ajax
import scala.concurrent
              .ExecutionContext
              .Implicits
              .global

import scala.scalajs.js
import scala.scalajs.js._
import edu.holycross.shot.cite._
import js.annotation._
import edu.holycross.shot.scm._
import edu.holycross.shot.citejson._
import edu.holycross.shot.citeobj._
import scala.scalajs.js.Dynamic.{ global => g }


@JSExportTopLevel("citeLexicon.MainModel")
object MainModel {

	val lexiconUrn:Cite2Urn = Cite2Urn("urn:cite2:hmt:ls.markdown:")
	val supportedVersion:String = "markdown"
	val sortProperty:Cite2Urn = Cite2Urn("urn:cite2:hmt:ls.markdown.seq:")


	val indexFile:String = "https://raw.githubusercontent.com/Eumaeus/cex_lewis_and_short/master/ls_indexData.txt"

	case class LexIndex( selector:String, ucodeKey:String, weight:Option[Int] = None)


	case class AlphaVolume( key:String, beta:String, selected:Boolean = false)

	val validLexUrn = Var[Boolean](false)
	val currentLexUrn = Var[Option[Cite2Urn]](None)
	val mainIndex = Var[Option[Vector[LexIndex]]](None)
	val shownIndex = Vars.empty[LexIndex]
	val selectedInShownIndex = Var[Option[String]](None)
	val alphaIndex = Vars.empty[AlphaVolume]
	val activeVolume = Var[Option[AlphaVolume]](None)
	val currentResults = Vars.empty[LexIndex]
    val requestParameterUrn = Var[Option[Cite2Urn]](None)

	val currentLexEntries = Vars.empty[CiteObject]

	def updateLexEntries(vco:Vector[CiteObject]):Unit = {
		currentLexEntries.value.clear
		for (le <- vco) { currentLexEntries.value += le }
	}
	def updateBubbles(vco:Vector[CiteObject]):Unit = {
		// show little tabs
		val justIds:Vector[String] = vco.map( _.urn.objectComponent )
		queryIndexFromFoundEntries(justIds)
	}

	def queryIndexFromFoundEntries(ids:Vector[String]) = {
		MainModel.mainIndex.value match {
			case None => // do nothing
				case Some(idx) => {
					val foundStuff:Vector[MainModel.LexIndex] = {
						idx.filter(i => ids.contains(i.selector))
					}
					//MainModel.clearSidebar
					MainModel.currentResults.value.clear
					for (i <- foundStuff) {
						MainModel.currentResults.value += i
					}
				}
		}
	}

	val alphaEntries:Vector[(String,String)] = {
		Vector(("Aa","a"), ("Bb","b"), ("Cc","c"), ("Dd","d"), ("Ee","e"), ("Ff","f"), ("Gg","g"), ("Hh","h"), ("Ii","i"), ("Jj","j"), ("Kk","k"), ("Ll","l"), ("Mm","m"), ("Nn","n"), ("Oo","o"), ("Pp","p"), ("Qq","q"), ("Rr","r"), ("Ss","s"), ("Tt","t"), ("Uu","u"), ("Vv","v"), ("Xx","x"), ("Yy","y"), ("Zz","z") )
	}


	def loadAlphaIndex(ae:Vector[(String,String)], selectedOne:Option[String] = None) = {
		alphaIndex.value.clear
		for (v <- alphaEntries) {
			selectedOne match {
				case Some(so) => {
					if (v._1 == so) alphaIndex.value += new AlphaVolume(v._1, v._2, true)
					else alphaIndex.value += new AlphaVolume(v._1, v._2, false)
				}
				case _ => alphaIndex.value += new AlphaVolume(v._1, v._2, false)

			}	
		}
	}

	def clearSidebar:Unit = {
		MainModel.shownIndex.value.clear 
		MainModel.activeVolume.value = None
		MainModel.clearActiveAlpha
		MainModel.selectedInShownIndex.value = None
	}

	def setActiveAlpha(s:String):Unit = {
		val newVolume:AlphaVolume = alphaIndex.value.filter(_.key == s).toVector(0)
		loadAlphaIndex(alphaEntries, Some(s))
		MainModel.changeShownIndex(newVolume)
	}

	def clearActiveAlpha:Unit = {
		loadAlphaIndex(alphaEntries, None)
	}

	def changeShownIndex(vol:AlphaVolume) = {
		mainIndex.value match {
			case None => shownIndex.value.clear
			case Some(i) => {
				val filteredIndex:Vector[LexIndex] = {
					i.filter(_.ucodeKey(0).toString.toLowerCase == vol.beta(0).toString.toLowerCase)
					//Vector()
				}
				shownIndex.value.clear
				for (i <- filteredIndex) {
					shownIndex.value += i
				}
			}
		}
	}



	/* Load LSJ Index String into a Vector[LexIndex] */
	def updateIndex(contents:String):Unit = {
		try {
			val tempIndex:Vector[LexIndex] = {
				contents.split("\n").toVector.map( is => {
					val selector = is.split("#")(0)
					val ucodeKey = is.split("#")(1)
					//val terms = is.split("#")(3)
					new LexIndex(selector, ucodeKey)
				}).toVector 
			}
			mainIndex.value = Some(tempIndex)
			MainController.updateUserMessage(s"Loaded index to lexicon: ${mainIndex.value.get.size} entries.",0)
			loadAlphaIndex(alphaEntries)
			// If there is a requst param, reload it now…
			MainModel.requestParameterUrn.value match {
				case Some(u) => MainController.initUrnQuery(u)
				case _ => // do nothing
			}
		} catch {
			case e:Exception => {
				MainController.updateUserMessage(s"Error loading index: ${e}", 2)
				g.console.log(s"${e}")
			}
		}
	}

	val serviceUrl= Var("")

	val userMessage = Var("Main loaded.")
	val userAlert = Var("default")
	val userMessageVisibility = Var("app_hidden")

	var msgTimer:scala.scalajs.js.timers.SetTimeoutHandle = null
	var typingTimer:scala.scalajs.js.timers.SetTimeoutHandle = null

	val lexCandidates = Vars.empty[MainModel.LexIndex]

	/* Should be in CiteLibrary: Given a URN and a property name, generate a property-level URN */
	def propertyUrnFromPropertyName(urn:Cite2Urn, propName:String):Cite2Urn = {
		val returnUrn:Cite2Urn = {
			urn.propertyOption match {
				case Some(po) => urn // just return it!
				case None => {
					val collUrn:Cite2Urn = urn.dropSelector
					val collUrnString:String = collUrn.toString.dropRight(1) // remove colon
					urn.objectComponentOption match {
						case Some(oc) => {
							Cite2Urn(s"${collUrnString}.${propName}:${oc}")
						}
						case None => {
							Cite2Urn(s"${collUrnString}.${propName}:")
						}
					}
				}
			}
		}
		returnUrn
	}	


	

}
