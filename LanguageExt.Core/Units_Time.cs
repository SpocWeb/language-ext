﻿using System;

namespace LanguageExt
{
    /// <summary>
    /// Numeric time-span value
    /// Handles unit conversions automatically
    /// All standard arithmetic operators work on the Time
    /// type.  So keep all Times wrapped until you need the
    /// value, then extract using various unit-of-measure
    /// accessors (Milliseconds, Seconds, etc.) or divide by 1.Second()
    /// Implicitly convertible to TimeSpan
    /// </summary>
    [Serializable]
    public struct Time :
        IAppendable<Time>,
        ISubtractable<Time>,
        IComparable<Time>,
        IEquatable<Time>
    {
        readonly double Value;

        internal Time(double length)
        {
            Value = length;
        }

        public override string ToString() =>
            Value + " s";

        public bool Equals(Time other) =>
            Value.Equals(other.Value);

        public bool Equals(Time other, double epsilon) =>
            Math.Abs(other.Value - Value) < epsilon;

        public override bool Equals(object obj) =>
            obj == null
                ? false
                : obj is Length
                    ? Equals((Length)obj)
                    : false;

        public override int GetHashCode() =>
            Value.GetHashCode();

        public int CompareTo(Time other) =>
            Value.CompareTo(other.Value);

        public Time Append(Time rhs) =>
            new Time(Value + rhs.Value);

        public Time Subtract(Time rhs) =>
            new Time(Value - rhs.Value);

        public Time Product(double rhs) =>
            new Time(Value * rhs);

        public Time Divide(double rhs) =>
            new Time(Value / rhs);

        public static Time operator *(Time lhs, double rhs) =>
            lhs.Product(rhs);

        public static Time operator *(double lhs, Time rhs) =>
            rhs.Product(lhs);

        public static Time operator /(Time lhs, double rhs) =>
            lhs.Divide(rhs);

        public static Time operator +(Time lhs, Time rhs) =>
            lhs.Append(rhs);

        public static Time operator -(Time lhs, Time rhs) =>
            lhs.Subtract(rhs);

        public static double operator /(Time lhs, Time rhs) =>
            lhs.Value / rhs.Value;

        public static bool operator ==(Time lhs, Time rhs) =>
            lhs.Equals(rhs);

        public static bool operator !=(Time lhs, Time rhs) =>
            !lhs.Equals(rhs);

        public static bool operator >(Time lhs, Time rhs) =>
            lhs.Value > rhs.Value;

        public static bool operator <(Time lhs, Time rhs) =>
            lhs.Value < rhs.Value;

        public static bool operator >=(Time lhs, Time rhs) =>
            lhs.Value >= rhs.Value;

        public static bool operator <=(Time lhs, Time rhs) =>
            lhs.Value <= rhs.Value;

        public Time Pow(double power) =>
            new Time(Math.Pow(Value, power));

        public Time Round() =>
            new Time(Math.Round(Value));

        public Time Sqrt() =>
            new Time(Math.Sqrt(Value));

        public Time Abs() =>
            new Time(Math.Abs(Value));

        public Time Min(Time rhs) =>
            new Time(Math.Min(Value, rhs.Value));

        public Time Max(Time rhs) =>
            new Time(Math.Max(Value, rhs.Value));

        public TimeSpan ToTimeSpan() =>
            TimeSpan.FromSeconds(Value);

        public static implicit operator TimeSpan(Time value) =>
            value.ToTimeSpan();

        public static implicit operator Time(TimeSpan value) =>
            new Time(value.TotalSeconds);

        public double Seconds       => Value;
        public double Milliseconds  => Value * 1000.0;
        public double Minutes       => Value / 60.0;
        public double Hours         => Value / 3600.0;
        public double Days          => Value / 86400.0;
    }

    namespace UnitsOfMeasure
    {
        public static class __UnitsTimeExt
        {
            public static Time Milliseconds(this int self) =>
                new Time(self / 1000.0);

            public static Time Milliseconds(this float self) =>
                new Time(self / 1000.0);

            public static Time Milliseconds(this double self) =>
                new Time(self / 1000.0);

            public static Time Seconds(this int self) =>
                new Time(self);

            public static Time Seconds(this float self) =>
                new Time(self);

            public static Time Seconds(this double self) =>
                new Time(self);

            public static Time Minutes(this int self) =>
                new Time(self * 60.0);

            public static Time Minutes(this float self) =>
                new Time(self * 60.0);

            public static Time Minutes(this double self) =>
                new Time(self * 60.0);

            public static Time Hours(this int self) =>
                new Time(self * 3600.0);

            public static Time Hours(this float self) =>
                new Time(self * 3600.0);

            public static Time Hours(this double self) =>
                new Time(self * 3600.0);

            public static Time Days(this int self) =>
                new Time(self * 86400.0);

            public static Time Days(this float self) =>
                new Time(self * 86400.0);

            public static Time Days(this double self) =>
                new Time(self * 86400.0);
        }
    }
}