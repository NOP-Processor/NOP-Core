// build.sc
import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

val spinalVersion = "1.8.0"

object NOP extends SbtModule {
  def scalaVersion = "2.12.16"
  override def mainClass = Some("NOP.Main")

  override def millSourcePath = os.pwd
  def sources = T.sources(
    millSourcePath / "src"
  )

  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:$spinalVersion",
    ivy"com.github.spinalhdl::spinalhdl-lib:$spinalVersion",
    ivy"com.github.scopt::scopt:4.0.1"
  )
  
  def scalacPluginIvyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:$spinalVersion",
    ivy"org.scalamacros:::paradise:2.1.1"
  )
}


