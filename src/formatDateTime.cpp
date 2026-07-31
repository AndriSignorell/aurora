#include <Rcpp.h>
#include <ctime>
#include <cmath>
#include <vector>
#include <string>
#include <clocale>
#include <memory>
#include <cctype>

using namespace Rcpp;

// --------------------------------------------------
// Token definition
// --------------------------------------------------
struct Token {
  std::string key;
  std::string strftime;
  bool strip_zero;
  bool manual;
};

// LONGEST match first!
const std::vector<Token> TOKENS = {
  {"dddd","%A",false,false},
  {"ddd","%a",false,false},
  {"MMMM","%B",false,false},
  {"MMM","%b",false,false},

  {"yyyy","",false,true},
  {"yy","%y",false,false},
  {"y","",true,true},

  {"do","",false,true},   // day with ordinal suffix

  {"dd","%d",false,false},
  {"d","%d",true,false},
  {"MM","%m",false,false},
  {"M","%m",true,false},

  {"HH","%H",false,false},
  {"H","%H",true,false},

  {"tt","%p",false,false},
  {"t","%p",true,false},

  {"hh","%I",false,false},
  {"h","%I",true,false},

  {"mm","%M",false,false},
  {"m","%M",true,false},
  {"ss","%S",false,false},
  {"s","%S",true,false}
};

// --------------------------------------------------
// helpers
// --------------------------------------------------
inline std::string strip_zero(const std::string& x) {
  if (x.size() > 1 && x[0] == '0')
    return x.substr(1);
  return x;
}

// Everything is broken down in UTC; the caller has already shifted the
// value into the wall-clock reading it wants (see .toWallClock() in
// fm.R). Three separate problems disappear with that split:
//
//   * A Date has no time zone. localtime() on its UTC midnight yields
//     the PREVIOUS day for every negative offset - the whole western
//     hemisphere saw fm(as.Date("2019-01-01"), "yyyy-MM-dd") as
//     "2018-12-31".
//   * localtime_r() is not required to consult TZ and does not call
//     tzset(), so a Sys.setenv(TZ = ...) had no effect here while
//     format() picked it up immediately.
//   * _tzset() on Windows understands only POSIX-style TZ strings, not
//     IANA names like "Europe/Zurich", and falls back to UTC when it
//     cannot parse one. Calling it made matters worse rather than
//     better.
//
// R's own tz database is authoritative and lives on the R side, so that
// is where the zone is applied.
inline bool break_down(time_t tt, std::tm& tm) {
#ifdef _WIN32
  return gmtime_s(&tm, &tt) == 0;
#else
  return gmtime_r(&tt, &tm) != nullptr;
#endif
}

// ordinal suffix helper
inline std::string ordinal_suffix(int d) {
  int last_two = d % 100;
  if (last_two >= 11 && last_two <= 13)
    return "th";

  switch (d % 10) {
  case 1: return "st";
  case 2: return "nd";
  case 3: return "rd";
  default: return "th";
  }
}

inline std::string eval_token(
    const Token& tk,
    const std::tm& tm,
    bool is_date
) {
  // manual tokens
  if (tk.manual) {
    int year = tm.tm_year + 1900;

    if (tk.key == "yyyy")
      return std::to_string(year);

    if (tk.key == "y")
      return std::to_string(year % 100);

    if (tk.key == "do") {
      int day = tm.tm_mday;
      return std::to_string(day) + ordinal_suffix(day);
    }
  }

  // time tokens on Date -> midnight
  if (is_date &&
      (tk.key == "H" || tk.key == "HH" ||
      tk.key == "m" || tk.key == "mm" ||
      tk.key == "s" || tk.key == "ss")) {
    return tk.strip_zero ? "0" : "00";
  }

  // ... but midnight on a 12-hour clock is 12, not 0. h/hh used to fall
  // into the branch above and produce "00:00 AM" where "12:00 AM"
  // belongs - the zero branch was written for the 24-hour tokens and
  // then applied to the 12-hour ones as well.
  if (is_date && (tk.key == "h" || tk.key == "hh")) {
    return "12";
  }

  // AM/PM on a Date is midnight, not a number: "00" used to come out
  // where "AM" belongs, e.g. fm(Sys.Date(), "hh:mm tt") -> "12:00 00"
  if (is_date && (tk.key == "t" || tk.key == "tt")) {
    char buf[16];
    std::tm midnight = tm;
    midnight.tm_hour = 0;
    size_t len = std::strftime(buf, sizeof(buf), "%p", &midnight);
    std::string val(buf, len);
    return tk.key == "t" ? val.substr(0, 1) : val;
  }

  char buf[64];
  // strftime returns 0 when the buffer is too small, and buf is then
  // indeterminate - std::string(buf) would read uninitialised memory
  size_t len = std::strftime(buf, sizeof(buf), tk.strftime.c_str(), &tm);
  std::string val(buf, len);

  // t = first letter of AM/PM
  if (tk.key == "t") {
    return val.empty() ? val : val.substr(0, 1);
  }

  if (tk.strip_zero)
    val = strip_zero(val);

  return val;
}

// --------------------------------------------------
// Locale guard (RAII)
// --------------------------------------------------
class LocaleGuard {
public:
  explicit LocaleGuard(const std::string& loc) {
    const char* old = std::setlocale(LC_TIME, nullptr);
    old_ = old ? old : "";
    std::setlocale(LC_TIME, loc.c_str());
  }
  ~LocaleGuard() {
    if (!old_.empty())
      std::setlocale(LC_TIME, old_.c_str());
  }
private:
  std::string old_;
};

// --------------------------------------------------
// Compiled format
// --------------------------------------------------
// The format string used to be re-scanned against the 22-entry token
// table for EVERY element of x. It does not change between elements, so
// it is parsed once here. The strict checks move out of the element loop
// with it, which is why they now also fire for a zero-length x.
//
// The 12-hour check is token-aware as a side effect. That is tidier but
// not a fix: neither 'h' nor 't' occurs inside any other token, and the
// format language has no way to write literal text, so the old
// fmt.find("h") could not actually misfire.
struct Piece {
  int token;      // index into TOKENS, or -1 for a literal character
  char literal;
};

static std::vector<Piece> compile_format(const std::string& fmt, bool strict) {

  std::vector<Piece> pieces;
  pieces.reserve(fmt.size());

  for (size_t pos = 0; pos < fmt.size();) {

    // ---- CORRECT yyy rejection (token-level) ----
    if (strict &&
        fmt.compare(pos, 3, "yyy") == 0 &&
        fmt.compare(pos, 4, "yyyy") != 0) {
      stop("Invalid format token 'yyy'. Did you mean 'yyyy'?");
    }

    bool matched = false;

    for (size_t k = 0; k < TOKENS.size(); ++k) {
      const Token& tk = TOKENS[k];
      if (fmt.compare(pos, tk.key.size(), tk.key) == 0) {
        Piece p; p.token = static_cast<int>(k); p.literal = '\0';
        pieces.push_back(p);
        pos += tk.key.size();
        matched = true;
        break;
      }
    }

    if (!matched) {
      if (strict && std::isalpha(static_cast<unsigned char>(fmt[pos]))) {
        stop("Unknown format token starting at '%c'", fmt[pos]);
      }
      Piece p; p.token = -1; p.literal = fmt[pos];
      pieces.push_back(p);
      pos++;
    }
  }

  return pieces;
}

static bool uses_token(const std::vector<Piece>& pieces,
                       const std::string& key) {
  for (size_t i = 0; i < pieces.size(); ++i)
    if (pieces[i].token >= 0 && TOKENS[pieces[i].token].key == key)
      return true;
  return false;
}

// Note the name: this is the UTC-only kernel, not the entry point.
// formatDateTime() is an R function in fm.R that applies the time zone
// first (see .toWallClock()) and then calls this. Keeping the old name
// here and adding an R function of the same name would put two
// definitions of formatDateTime into one namespace, with the winner
// decided by collation order.
// [[Rcpp::export]]
CharacterVector formatDateTimeUtc(
    SEXP x,
    std::string fmt,
    bool strict = true,
    std::string locale = "current"
) {

  R_xlen_t n = Rf_xlength(x);
  CharacterVector out(n);

  // inherits() has to be asked BEFORE the coercion below, which drops
  // the class attribute
  bool is_date = Rf_inherits(x, "Date");

  // A Date is only required to be numeric, not double: seq.Date() hands
  // off to seq.int() and returns INTEGER storage for whole endpoints, so
  // seq(d1, d2, by = "days") arrived here as an INTSXP and REAL() threw
  // "REAL() can only be applied to a 'numeric', not a 'integer'".
  // RObject rather than PROTECT/UNPROTECT: stop() below throws, and the
  // destructor has to run for the protection to be released.
  RObject xr(x);
  if (TYPEOF(x) != REALSXP)
    xr = Rf_coerceVector(x, REALSXP);
  const double* xp = REAL(xr);

  // --------------------------
  // locale handling
  // --------------------------
  std::unique_ptr<LocaleGuard> lg;
  if (locale != "current") {
    lg.reset(new LocaleGuard(locale == "C" ? "C" : locale));
  }

  // --------------------------
  // parse the format once
  // --------------------------
  const std::vector<Piece> pieces = compile_format(fmt, strict);

  // --------------------------
  // strict pre-check: 12h needs AM/PM
  // --------------------------
  if (strict) {
    bool uses_12h  = uses_token(pieces, "h") || uses_token(pieces, "hh");
    bool has_ampm  = uses_token(pieces, "t") || uses_token(pieces, "tt");

    if (uses_12h && !has_ampm) {
      stop("12-hour format ('h' or 'hh') requires 't' or 'tt' (AM/PM designator)");
    }
  }

  // --------------------------
  // warning: English-only ordinal token
  // --------------------------
  if (uses_token(pieces, "do")) {

    std::string effective_locale = locale;

    if (locale == "current") {
      const char* loc = std::setlocale(LC_TIME, nullptr);
      if (loc)
        effective_locale = loc;
    }

    if (effective_locale.rfind("en", 0) != 0 && effective_locale != "C"
            && effective_locale != "POSIX") {
      Rcpp::warning(
        "Token 'do' (1st, 2nd, ...) only makes sense for English locales. "
        "Consider using lang=\"en\"."
      );
    }
  }

  // --------------------------
  // vectorized formatting
  // --------------------------
  for (R_xlen_t i = 0; i < n; ++i) {

    // R_FINITE rather than ISNA: ISNA() is false for NaN, and an Inf
    // would reach static_cast<time_t>() below, which is undefined
    // behaviour for a value outside the target range
    if (!R_FINITE(xp[i])) {
      out[i] = NA_STRING;
      continue;
    }

    // floor(), and the multiplication in double before the cast: casting
    // first truncated towards zero, which is off by a day for fractional
    // dates before 1970, and days * 86400 could overflow a 32-bit time_t
    double secs = is_date ? std::floor(xp[i]) * 86400.0 : xp[i];

    std::tm tm;
    if (!break_down(static_cast<time_t>(secs), tm)) {
      out[i] = NA_STRING;
      continue;
    }

    std::string res;
    for (size_t p = 0; p < pieces.size(); ++p) {
      if (pieces[p].token >= 0)
        res += eval_token(TOKENS[pieces[p].token], tm, is_date);
      else
        res.push_back(pieces[p].literal);
    }

    out[i] = res;
  }

  return out;
}
