import sbt._

object Dependencies {
  object Versions {
    val cats          = "2.2.0"
    val catsEffect    = "2.2.0"
    val catsMeowMtl   = "0.4.1"
    val fs2           = "2.4.5"
    val newtype       = "0.4.3"
    val refined       = "0.9.18"

    val betterMonadicFor = "0.3.1"
    val kindProjector    = "0.11.0"
    val logback          = "1.2.3"

    val scalaCheck    = "1.15.1"
    val scalaTest     = "3.2.3"
    val scalaTestPlus = "3.2.2.0"
  }

  object Libraries {

    val cats        = "org.typelevel"    %% "cats-core"     % Versions.cats
    val catsMeowMtl = "com.olegpy"       %% "meow-mtl-core" % Versions.catsMeowMtl
    val catsEffect  = "org.typelevel"    %% "cats-effect"   % Versions.catsEffect
    val fs2         = "co.fs2"           %% "fs2-core"      % Versions.fs2


    val refinedCore = "eu.timepit" %% "refined"      % Versions.refined
    val refinedCats = "eu.timepit" %% "refined-cats" % Versions.refined

    val newtype  = "io.estatico"       %% "newtype"        % Versions.newtype


    // Compiler plugins
    val betterMonadicFor = "com.olegpy"    %% "better-monadic-for" % Versions.betterMonadicFor
    val kindProjector    = "org.typelevel" % "kind-projector"      % Versions.kindProjector

    // Runtime
    val logback = "ch.qos.logback" % "logback-classic" % Versions.logback

    // Test
    val scalaCheck    = "org.scalacheck"    %% "scalacheck"      % Versions.scalaCheck
    val scalaTest     = "org.scalatest"     %% "scalatest"       % Versions.scalaTest
    val scalaTestPlus = "org.scalatestplus" %% "scalacheck-1-14" % Versions.scalaTestPlus
  }


}
