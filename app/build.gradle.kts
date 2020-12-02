plugins {
    scala
    application
}

repositories {
    jcenter()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.13.3")
    implementation("com.google.guava:guava:29.0-jre")

}

application {
    mainClass.set("aoc2020.App")
}
