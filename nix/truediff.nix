{
  buildSbtPackage,
  fetchFromGitLab,
}:
buildSbtPackage {
  pname = "truediff";
  version = "0.2.0-SNAPSHOT";
  src = fetchFromGitLab {
    domain = "gitlab.rlp.net";
    owner = "plmz";
    repo = "truediff";
    rev = "e540bd251b9a0fa5ff019595577f7ac2abca74dc";
    hash = "sha256-Tz7PlgMLakRhBxDZxHppjTWMQhP5f5bK8Ve9hILXUn0=";
  };

  dependenciesHash = "sha256-OUNhilRJN4isnNRUoHAXOs24FTm7JpdVcOiQL5fTCVw=";
}
