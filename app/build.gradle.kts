plugins {
    scala
    application
}

repositories {
    jcenter()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.13.4")
}

application {
    mainClass.set("aoc2020.App")
}
