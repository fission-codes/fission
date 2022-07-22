-- OLD type: TEXT
-- NEW type: MAP, example => [["type","sEd25519PublicKey"],["key","sHv+AVRD2WUjUFOsSNbsmrp9fokuwrUnjBcr92f0kxw4="]]=
--
UPDATE "user" SET public_key = (
  CASE

  -- Already migrated?
  WHEN starts_with(public_key, '[') THEN
    public_key

  -- Before this migration was created we only had ED25519 and RSA keys
  WHEN starts_with(public_key, 'MII') THEN
    format('[["type","s%s"],["key","s%s"]]', 'RSAPublicKey', public_key)

  ELSE
    format('[["type","s%s"],["key","s%s"]]', 'Ed25519PublicKey', public_key)

  END
)