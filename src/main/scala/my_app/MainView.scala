package citeLexicon 

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import scala.scalajs.js.Dynamic.{ global => g }
import org.scalajs.dom.raw._
import org.scalajs.dom.document
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext.Ajax
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.greek._
import edu.holycross.shot.citeobj._
import scala.scalajs.js.annotation.JSExport
import js.annotation._
import scala.concurrent._
import monix.execution.Scheduler.Implicits.global
import monix.eval._


@JSExportTopLevel("citeLexicon.MainView")
object MainView {


	@dom
	def mainMessageDiv = {
			<div id="main_message" class={ s"app_message ${MainModel.userMessageVisibility.bind} ${MainModel.userAlert.bind}" } >
				<p> { MainModel.userMessage.bind }  </p>
			</div>
	}



	@dom
	def mainDiv = {
		<div id="main-wrapper">
		<header>
			<em>A Latin Dictionary.</em> Founded on Andrews’ edition of Freund's Latin dictionary, revised, enlarged, and in great part rewritten by Charlton T. Lewis, Ph.D., and Charles Short, LL.D. (Oxford: Clarendon Press, 1879)
			<span id="app_header_versionInfo">CITE Version { BuildInfo.version }</span>
			{ MainView.mainMessageDiv.bind }
		</header>

		<article id="main_Container">

		<p id="menu"> <a href="http://cite-architecture.github.io">The CITE Archtecture</a> | <a href="https://eumaeus.github.io/2018/10/30/lsj.html">About this project</a> </p>

		
		{ MainView.alphaList.bind }
		{ MainView.volumeList.bind }
		<div id="lexiconDiv">
			{ MainView.searchDiv.bind }
			{ MainView.resultsDiv.bind  }
			{ MainView.entryDiv.bind  }
		</div>
		<textarea id="hiddenTextArea"></textarea>
		</article>
		 <div class="push"></div>
		<footer>
		{ footer.bind }
		</footer>
	</div>
	}

	@dom
	def volumeList = {
		<ul id="volumeList" class={
			if (MainModel.shownIndex.length.bind > 20) {
				"greekFont long"
			} else {
				"greekFont"
			}
		}>

			 {
				for (v <- MainModel.shownIndex ) yield {
					<li id={ s"entry_${v.selector}"}
						class={
							MainModel.selectedInShownIndex.bind match {
								case Some(i) => {
									if (v.selector == i) "selected" else ""
								}
								case _ => ""
							}
						}
						onclick={ event: Event => {
							MainController.initLsjSingleQuery(v.selector)
						}}
						>{ v.ucodeKey}</li>
				}
			}
		</ul>
	}

	@dom
	def resultsDiv = {
		<ul id="resultsList">
			{
				for ( r <- MainModel.currentResults) yield {
					<li id={ s"lexResultListItem_${r.selector}" }
						onclick={ event: Event => {
							MainController.initLsjSingleQuery(r.selector)
						}}
						class={ 
							MainModel.selectedInShownIndex.bind match {
								case Some(i) => {
									if (r.selector == i) "lexResultListItem selected" else ""
								}
								case _ => "lexResultListItem"
							}
						} >
						{ s"${r.ucodeKey}"}
					</li>
				}
			}
		</ul>		
	}

	@dom
	def entryDiv = {
		<div id="entryDiv" class="greekFont">
			{
				for (lex <- MainModel.currentLexEntries) yield {
					val thisUrn:Cite2Urn = lex.urn
					val thisLabel:String = lex.label
					val entryPropertyUrn:Cite2Urn = MainModel.propertyUrnFromPropertyName(thisUrn, "entry")
					val thisEntry:String = lex.propertyValue(entryPropertyUrn).asInstanceOf[String]
					initiateMarked(s"${thisEntry}", s"lexEntry_${thisUrn}")
					<div id={ s"lexEntryContainerDiv_${thisUrn}"} class="lexEntryDiv">
						<p class="lexEntryUrnP">{ lexUrnElement(s"${thisUrn}" ).bind }</p>
						<p class="lexEntryLabel">{ s"${thisLabel}" }</p>
						<p class="lexEntry" id={ s"lexEntry_${thisUrn}" }>{ s"${thisEntry}" }</p>
					</div>

				}

			}
		</div>		
	}

	@dom
	def lexUrnElement(u:String) = {
		
		<span class="lexEntryUrn" 
			id={ "entryUrn_${s}" }>
			<a
			  onclick={ event: Event => {
				val thisTarget = event.target.asInstanceOf[org.scalajs.dom.raw.HTMLAnchorElement]
				val selectedText:String = thisTarget.text
			    val hiddenTextArea = js.Dynamic.global.document.getElementById("hiddenTextArea")
			    hiddenTextArea.textContent = selectedText.trim
			    hiddenTextArea.select()
			    val successful = document.execCommand("copy")
				successful match {
					case true => {
						val message:String = s"""Copied "${selectedText}" to clipboard."""
						MainController.updateUserMessage(message, 0)
					}
					case false => {
						val message:String = s"""Failed to copy "${selectedText}" to clipboard."""
						MainController.updateUserMessage(message, 2)
					}
				}
			}}	> { u } </a></span>

	}
	
	@dom
	def alphaList = {
		<ul id="alphaList" class="greekFont">

			 {
				for (a <- MainModel.alphaIndex ) yield {
					<li id={ s"alpha_${a.key}"}
						class={ 
							if (a.selected) "selected" else ""
						}
						onclick={ event: Event => {
							MainModel.setActiveAlpha(a.key) 
						}}
						>{ a.key}</li>
				}
			}
		</ul>
	}

	@dom
	def searchDiv = {
		
		<div>
		{ greekSearchForm.bind }	
		{ englishSearchForm.bind }	
		{ urnSearchForm.bind }
		</div>
		
	}

	@dom 
	def greekSearchForm = {
		val greekKeyUpHandler = { event: KeyboardEvent =>
			(event.currentTarget, event.keyCode) match {
				case(input: HTMLInputElement, _) =>  {
					if (input.value.size > 1){
						MainController.queryIndex(input.value.toString)
					} else {
						MainModel.currentResults.value.clear
					}
				}
				case _ =>
			}
		}
		<div id="searchDiv">
			<label class="inputLabel" for="greekInput">Look up: </label>
			<input
				class={ s"greekInputField" }
				id="greekInput"
				size={ 30 }
				value=""
				onkeyup={ greekKeyUpHandler }>
			</input>	
		</div>
	}

	@dom
	def englishSearchForm = {
		<div id="searchEnglishDiv">
			<label class="inputLabel" for="englishInput">Search All Text: </label>
			<input
				class={ s"greekInputField" }
				id="englishInput"
				size={ 30 }
				value="">
			</input>	
			<button 
				id="searchButton"
				onclick={ event: Event => {
					MainController.updateUserMessage("Finding text in passages. Please be patient…",1)
					val thisText:String = js.Dynamic.global.document.getElementById("englishInput").value.toString
						
					val task = Task{MainController.initTextQuery(thisText)}	
					val future = task.runAsync
				}
			}
			>Search All Text</button>
		</div>	
	}	

	@dom
	def urnSearchForm = {

		val urnValidatingKeyUpHandler = { event: KeyboardEvent =>
			(event.currentTarget, event.keyCode) match {
				case (input: html.Input, KeyCode.Enter) => {
					event.preventDefault()
				}
				case(input: html.Input, _) =>  {
					val validPassage:Boolean = MainController.validatePassage(s"${input.value.toString}")
					validPassage match {
						case false => {
							MainModel.validLexUrn.value = false
						}
						case true => {
							MainModel.validLexUrn.value = true
						}
					}
				}
				case _ =>
			}
		}
		<div id="passageInputDiv">
			<label  for="passageInput"
				class={
					MainModel.validLexUrn.bind match {
						case true => "validPassage inputLabel"
						case false => "invalidPassage inputLabel"
					}					
				}	
			> Retrieve by URN: </label>
			<input id="passageInput" type="text" size={30} placeholder="urn:cite2:hmt:ls.markdown:n147"
				class={
					MainModel.validLexUrn.bind match {
						case true => "validPassage"
						case false => "invalidPassage"
					}					
				}	
				onkeyup={ urnValidatingKeyUpHandler }
			/>
			{ queryButton.bind }
			
		</div>
	}



	@dom
	def queryButton = {
	<button
		id="querySubmit"
		disabled={ MainModel.validLexUrn.bind == false }
			onclick={ event: Event => {
					MainController.updateUserMessage("Finding entry for URN. Please be patient…",1)
					val thisEntry:String = js.Dynamic.global.document.getElementById("passageInput").value.toString
					g.console.log(thisEntry)	
					val task = Task{MainController.initUrnQuery(thisEntry)}	
					val future = task.runAsync
				}
			}
		>{ if (MainModel.validLexUrn.bind){
			"Retrieve Lexicon Entries"
		} else {
			"Enter a valid URN"
		}
			}
	</button>
	}

	@dom
	def waitingOn = {
		val docBody = js.Dynamic.global.document.getElementById("body")
		docBody.setAttribute("class","loading")
	}

	@dom
	def waitingOff = {
		val docBody = js.Dynamic.global.document.getElementById("body")
		docBody.setAttribute("class","")
	}

	@dom
	def scrollToItemInSidebar(idString:String):Unit = {
		val ulElement = js.Dynamic.global.document.getElementById("volumeList").asInstanceOf[HTMLUListElement]
		val liElement = js.Dynamic.global.document.getElementById(idString).asInstanceOf[HTMLLIElement]
		ulElement.scrollTop = (liElement.offsetTop - 150)
	}

	@dom
	def footer = {
		<p> <em>A Latin Dictionary.</em> Founded on Andrews’ edition of Freund's Latin dictionary, revised, enlarged, and in great part rewritten by Charlton T. Lewis, Ph.D., and Charles Short, LL.D. (Oxford: Clarendon Press, 1879) Text provided by Perseus Digital Library, with funding from The National Endowment for the Humanities. Original version available for viewing and download at <a href="http://www.perseus.tufts.edu/">http://www.perseus.tufts.edu/</a>. This application is ©2018, Christopher W. Blackwell, licensed under the <a href="https://www.gnu.org/licenses/gpl-3.0.en.html">GPL 3.0</a>. CITE/CTS is ©2002–2018 Neel Smith and Christopher Blackwell. The implementations of the <a href="http://cite-architecture.github.io">CITE</a> were written by Neel Smith and Christopher Blackwell using <a href="https://www.scala-lang.org">Scala</a>, <a href="http://www.scala-js.org">Scala-JS</a>, and <a href="https://github.com/ThoughtWorksInc/Binding.scala">Binding.scala</a>. Licensed under the <a href="https://www.gnu.org/licenses/gpl-3.0.en.html">GPL 3.0</a>. The dictionary data is <a href="https://github.com/Eumaeus/cex_lewis_and_short">is on Github</a>. Sourcecode on <a href="https://github.com/Eumaeus/cite-lsj-browser">GitHub</a>. Report bugs by <a href="https://github.com/Eumaeus/cite-lsj-browser/issues">filing issues on GitHub.</a>
		</p>
	}


	/* Methods for connecting out to Javascript */
	@JSGlobal("initiateMarked")
	@js.native
	object initiateMarked extends js.Any {
		def apply(markdownString:String, elementId:String): js.Dynamic = js.native
	}


}
