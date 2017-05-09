def env(key: String) = Option(System.getenv(key)).getOrElse("")

pgpPassphrase := Some(env("PGP_PASSPHRASE").toArray)

pgpPublicRing := file("admin/pubring.asc")

pgpSecretRing := file("admin/secring.asc")

credentials   += Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", env("SONA_USER"), env("SONA_PASS"))
