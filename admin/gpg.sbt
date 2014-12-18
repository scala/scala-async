
addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3") // only added when publishing:

// There's a companion sensitive.sbt, which was created like this:
//
// 1. in an sbt shell that has the sbt-pgp plugin, create pgp key in admin/:
//
// sbt
//  set pgpReadOnly := false
//  set pgpPublicRing := file("admin/pubring.asc")
//  set pgpSecretRing := file("admin/secring.asc")
//  pgp-cmd gen-key // use $passPhrase
// 	 Please enter the name associated with the key: $repoName
// 	 Please enter the email associated with the key: scala-internals@googlegroups.com
// 	 Please enter the passphrase for the key: $passphrase
//
// 2. create sensitive.sbt with contents:
//
// pgpPassphrase := Some($passPhrase.toArray)
//
// pgpPublicRing := file("admin/pubring.asc")
//
// pgpSecretRing := file("admin/secring.asc")
//
// credentials   += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", $sonaUser, $sonaPass)

