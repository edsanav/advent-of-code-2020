package exercises

import cats.effect.Sync
import scala.util.matching.Regex

object day4 {

  type Passport = Map[String, String]
  val COMPULSORY: List[String] = List("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")
  val OPTIONAL: List[String]   = List("cid")
  val ALL: List[String]        = OPTIONAL ::: COMPULSORY

  def asPairOption(s: String): Option[(String, String)] = s.split(":") match {
    case Array(k, v) => Some((k, v))
    case _           => None
  }

  def validStructure(pp: Passport): Boolean = pp.keys.count(COMPULSORY.contains(_)) == COMPULSORY.length

  def isInteger(s: String): Boolean = s.toIntOption.isDefined

  def inRange(x: Int, lower: Int, upper: Int): Boolean = lower <= x && x <= upper

  def endsWith(s: String, valid: List[String]): Boolean = valid.exists(ss => s.endsWith(ss))

  def validPattern(s: String, pattern: Regex): Boolean = pattern.findFirstIn(s).isDefined

  def validByr(s: String): Boolean = isInteger(s) && inRange(s.toInt, 1920, 2002)

  def validIyr(s: String): Boolean = isInteger(s) && inRange(s.toInt, 2010, 2020)

  def validEyr(s: String): Boolean = isInteger(s) && inRange(s.toInt, 2020, 2030)

  def validHgt(s: String): Boolean =
    if (s.endsWith("cm")) {
      val x = s.stripSuffix("cm")
      isInteger(x) && inRange(x.toInt, 150, 193)
    } else if (s.endsWith("in")) {
      val x = s.stripSuffix("in")
      isInteger(x) && inRange(x.toInt, 59, 76)
    } else {
      false
    }

  val HCL_PATTERN: Regex           = "^#[a-z0-9]{1,6}$".r
  def validHcl(s: String): Boolean = validPattern(s, HCL_PATTERN)

  val EYE_COLORS                   = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  def validEcl(s: String): Boolean = EYE_COLORS.contains(s)

  val PID_PATTERN: Regex           = "^[0-9]{9}$".r
  def validPid(s: String): Boolean = validPattern(s, PID_PATTERN)

  val RULES_REF: Map[String, String => Boolean] = Map(
    "byr" -> validByr,
    "iyr" -> validIyr,
    "eyr" -> validEyr,
    "hgt" -> validHgt,
    "hcl" -> validHcl,
    "ecl" -> validEcl,
    "pid" -> validPid
  )

  def isValidFromRules(ruleKeys: List[String])(passport: Passport): Boolean =
    ruleKeys.forall(key => {
      val rule = RULES_REF.getOrElse(key, (_: String) => { true })
      (passport.contains(key) && rule(passport(key)))
    })

  def run[F[_]: Sync](in: Iterator[String]): F[Unit] = {
    val input     = in.mkString(" ").split("  ").to(LazyList)
    val passports = input.map(_.split(" ").flatMap(asPairOption).toMap).filter(validStructure)
    val isValidBasicRules: (Passport => Boolean) = isValidFromRules(
      List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    )
    val result1 = passports.size
    val result2 = passports.count(isValidBasicRules)
    Sync[F].delay(println((result1, result2)))
  }

}
